--
--  Copyright (C) 2023-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Assertions;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Prettier_Ada.Documents.Builders;
with Prettier_Ada.Optionals;

with VSS.Characters.Latin;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.Strings.Markers;
with VSS.Strings.Utilities;

package body Prettier_Ada.Documents.Implementation is

   use type VSS.Strings.Display_Cell_Count;

   Current_Symbol : Symbol_Type := No_Symbol + 1;

   Document_Id : Natural := Natural'First;

   package Optional_Booleans is new
     Prettier_Ada.Optionals.Generic_Optional_Types (Boolean);

   subtype Optional_Boolean is Optional_Booleans.Generic_Optional_Type;

   subtype Document_Vector is Prettier_Ada.Document_Vectors.Vector;

   package Document_Hashed_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Document_Type,
        Hash                => Hash,
        Equivalent_Elements => "=");

   subtype Document_Hashed_Set is Document_Hashed_Sets.Set;

   type Mode_Kind is (Mode_Break, Mode_Flat, None);

   type Indentation_Data_Kind is
     (Indent, String_Align, Number_Align, Dedent, Inner_Root);

   type Indentation_Data_Type (Kind : Indentation_Data_Kind := Indent) is
      record
         case Kind is
            when Indent | Dedent =>
               null;
            when Inner_Root =>
               Margin : Natural;
            when String_Align =>
               Text   : Prettier_String;
            when Number_Align =>
               Width  : Natural;
         end case;
      end record;

   package Indentation_Data_Vectors is new
     Ada.Containers.Vectors (Natural, Indentation_Data_Type);

   subtype Indentation_Data_Vector is Indentation_Data_Vectors.Vector;

   type Indentation_Queue_Type;

   type Indentation_Head_Type_Access is access Indentation_Queue_Type;

   type Indentation_Queue_Type is record
      Value : Prettier_String;
      Queue : Indentation_Data_Vector;
      Root  : Indentation_Head_Type_Access;
   end record;

   type Print_Command_Type is record
      Indentation : Indentation_Queue_Type;
      Mode        : Mode_Kind;
      Document    : Document_Type;
   end record;

   package Print_Command_Type_Vectors is new
     Ada.Containers.Vectors (Positive, Print_Command_Type);

   subtype Print_Command_Type_Vector is Print_Command_Type_Vectors.Vector;

   package Symbol_To_Mode_Maps is new
     Ada.Containers.Hashed_Maps (Symbol_Type, Mode_Kind, Hash, "=");

   subtype Symbol_To_Mode_Map is Symbol_To_Mode_Maps.Map;

   procedure Append (To : in out Prettier_String; Source : Prettier_String);
   --  Append Source to the end of To

   procedure Break_Parent_Group
     (Group_Stack : in out Document_Vector);
   --  TODO: Description

   function Fits
     (Next            : Print_Command_Type;
      Rest_Commands   : Print_Command_Type_Vector;
      Width           : Integer;
      Has_Line_Suffix : Boolean;
      Group_Mode_Map  : Symbol_To_Mode_Map;
      Must_Be_Flat    : Boolean := False)
      return Boolean;
   --  TODO: Description

   function Generate_Indentation
     (From    : Indentation_Queue_Type;
      Data    : Indentation_Data_Type;
      Options : Indentation_Options_Type)
      return Indentation_Queue_Type;
   --  TODO: Description

   function Get_Document_Parts
     (Document : Document_Type)
      return Document_Vector;
   --  TODO: Add the following contract:
   --  with Pre => Document.Kind = Document_List
   --              or else (Document.Kind = Document_Command
   --                       and then Document.Bare_Document.Command.Kind
   --                                = Command_Fill);
   --  TODO: Description

   function Make_Align
     (From                : Indentation_Queue_Type;
      Align_Data          : Alignment_Data_Type;
      Options             : Format_Options_Type;
      Current_Line_Length : Natural)
      return Indentation_Queue_Type;
   --  TODO: Description

   function Make_Indentation
     (From    : Indentation_Queue_Type;
      Options : Indentation_Options_Type)
      return Indentation_Queue_Type;
   --  TODO: Description

   procedure Propagate_Breaks (Document : Document_Type);
   --  TODO: Description

   function Root_Indent return Indentation_Queue_Type;
   --  TODO: Description

   function Slice
     (Documents : Document_Vector;
      First     : Positive;
      Last      : Positive)
      return Document_Vector;
   --  TODO: Description

   procedure Traverse_Document
     (Document                           : Document_Type;
      On_Enter                           :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      On_Exit                            :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      Should_Traverse_Conditional_Groups : Boolean := False);
   --  TODO: Description
   --  TODO: Refactor so that Optional_Boolean is not needed

   type Trim_End is (Left, Right, Both);

   procedure Trim
     (Item : in out Prettier_String;
      Side : Trim_End);
   --  Remove whitespaces and horizontal tab characters from Item.
   --  Side controls from which end(s) characters are removed.

   function Trim
     (Text : in out Prettier_String)
      return Natural;
   --  Removes leading whitespaces and horizontal tab characters from Text.
   --  Returns how many characters were removed.

   function "-"
     (Left  : Integer;
      Right : VSS.Strings.Display_Cell_Count) return Integer is
      (Left - Integer (Right));

   function "+"
     (Left  : Integer;
      Right : VSS.Strings.Display_Cell_Count) return Integer is
      (Left + Integer (Right));

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Document_Type) return Boolean is
     ((Left.Bare_Document = null and Right.Bare_Document = null)
      or (Left.Bare_Document /= null
          and then Right.Bare_Document /= null
          and then Left.Bare_Document.Id = Right.Bare_Document.Id));

   ------------
   -- Append --
   ------------

   procedure Append (To : in out Prettier_String; Source : Prettier_String)
   is
   begin
      VSS.Strings.Append (To.Text, Source.Text);
      To.Display_Width := @ + Source.Display_Width;
   end Append;

   ------------------------
   -- Break_Parent_Group --
   ------------------------

   procedure Break_Parent_Group
     (Group_Stack : in out Document_Vector)
   is
      use type Ada.Containers.Count_Type;

   begin
      if Group_Stack.Length > 0 then
         declare
            Parent_Group  : constant Document_Vectors.Reference_Type :=
              Group_Stack.Reference (Group_Stack.Last);
            Group_Command : constant Command_Access :=
              Parent_Group.Bare_Document.Command;

         begin
            if (Group_Command.Expanded_States = No_Document
                or else Group_Command.Expanded_States.Bare_Document.List.Length
                        = 0)
              and not Group_Command.Break
            then
               --  TODO: Why should this be "propagated"?
               Gnatfmt_Trace.Trace ("BPG");
               Group_Command.all.Break := True;
            end if;
         end;
      end if;
   end Break_Parent_Group;

   ----------
   -- Fits --
   ----------

   function Fits
     (Next            : Print_Command_Type;
      Rest_Commands   : Print_Command_Type_Vector;
      Width           : Integer;
      Has_Line_Suffix : Boolean;
      Group_Mode_Map  : Symbol_To_Mode_Map;
      Must_Be_Flat    : Boolean := False)
      return Boolean
   is
      type Fit_Command_Type is record
         Mode     : Mode_Kind;
         Document : Document_Type;
      end record;

      package Fit_Command_Type_Vectors is new
        Ada.Containers.Vectors (Positive, Fit_Command_Type);

      subtype Fit_Command_Type_Vector is Fit_Command_Type_Vectors.Vector;

      function To_Fit_Command_Type
        (Print_Command : Print_Command_Type)
         return Fit_Command_Type;
      --  TODO: Description

      -------------------------
      -- To_Fit_Command_Type --
      -------------------------

      function To_Fit_Command_Type
        (Print_Command : Print_Command_Type)
         return Fit_Command_Type
      is (Fit_Command_Type'(Print_Command.Mode, Print_Command.Document));

      function "+" (Print_Comment : Print_Command_Type) return Fit_Command_Type
        renames To_Fit_Command_Type;

      Remaining_Width : Integer := Width;
      Rest_Idx        : Print_Command_Type_Vectors.Extended_Index :=
        Rest_Commands.Last_Index;
      Fit_Commands    : Fit_Command_Type_Vector := [+Next];

      Current_Line : Prettier_String;

      Current_Has_Line_Suffix : Boolean := Has_Line_Suffix;

   begin
      Gnatfmt_Trace.Trace ("F1");
      if Remaining_Width = Integer'Last then
         Gnatfmt_Trace.Trace ("F2");
         return True;
      end if;

      while Remaining_Width >= 0 loop
         Gnatfmt_Trace.Trace ("F3");
         if Fit_Commands.Is_Empty then
            Gnatfmt_Trace.Trace ("F4");
            if Rest_Idx < Rest_Commands.First_Index then
               Gnatfmt_Trace.Trace ("F5");
               return True;
            end if;

            Fit_Commands.Append (+Rest_Commands.Element (Rest_Idx));
            Rest_Idx := @ - Natural (1);

         else
            Gnatfmt_Trace.Trace ("F6");
            declare
               Current_Fit_Command : constant Fit_Command_Type :=
                  Fit_Commands.Last_Element;
               Mode                : Mode_Kind
                 renames Current_Fit_Command.Mode;
               Document            : Document_Type
                 renames Current_Fit_Command.Document;

            begin
               Gnatfmt_Trace.Trace ("F7");
               Fit_Commands.Delete_Last;

               case Document.Bare_Document.Kind is
                  when Document_Text =>
                     Gnatfmt_Trace.Trace ("F8");
                     Append
                       (Current_Line, Document.Bare_Document.Text);
                     Remaining_Width :=
                       @ - Document.Bare_Document.Text.Display_Width;

                  when Document_List =>
                     Gnatfmt_Trace.Trace ("F9");
                     for Child_Document of
                        reverse Get_Document_Parts (Document)
                     loop
                        Gnatfmt_Trace.Trace ("F10");
                        Fit_Commands.Append
                          (Fit_Command_Type'(Mode, Child_Document));
                     end loop;

                  when Document_Command =>
                     case Document.Bare_Document.Command.Kind is
                        when Command_Fill =>
                           Gnatfmt_Trace.Trace ("F9");
                           for Child_Document of
                              reverse Get_Document_Parts (Document)
                           loop
                              Gnatfmt_Trace.Trace ("F10");
                              Fit_Commands.Append
                                (Fit_Command_Type'(Mode, Child_Document));
                           end loop;

                        when Command_Indent =>
                           Gnatfmt_Trace.Trace ("F11");
                           Ada.Assertions.Assert
                             (Document
                                .Bare_Document
                                .Command
                                .Indent_Contents
                              /= No_Document);
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Indent_Contents));

                        when Command_Align =>
                           Gnatfmt_Trace.Trace ("F11");
                           Ada.Assertions.Assert
                             (Document
                                .Bare_Document
                                .Command
                                .Align_Contents
                              /= No_Document);
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Align_Contents));

                        when Command_Indent_If_Break =>
                           Gnatfmt_Trace.Trace ("F11");
                           Ada.Assertions.Assert
                             (Document
                                .Bare_Document
                                .Command
                                .Indent_If_Break_Contents
                              /= No_Document);
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Indent_If_Break_Contents));

                        when Command_Label =>
                           Gnatfmt_Trace.Trace ("F11");
                           Ada.Assertions.Assert
                             (Document
                                .Bare_Document
                                .Command
                                .Label_Contents
                              /= No_Document);
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Label_Contents));

                        when Command_Trim =>
                           Gnatfmt_Trace.Trace ("F12");
                           Remaining_Width := @ + Trim (Current_Line);

                        when Command_Group =>
                           Gnatfmt_Trace.Trace ("F13");
                           if Must_Be_Flat
                             and Document.Bare_Document.Command.Break
                           then
                              Gnatfmt_Trace.Trace ("F14");
                              return False;
                           end if;

                           declare
                              use type Ada.Containers.Count_Type;

                              Group_Mode : constant Mode_Kind :=
                                (if Document.Bare_Document.Command.Break then
                                   Mode_Break
                                 else
                                   Mode);
                              Contents : constant Document_Type :=
                                (if (Document
                                      .Bare_Document
                                      .Command
                                      .Expanded_States
                                     /= No_Document
                                     and then Document
                                      .Bare_Document
                                      .Command
                                      .Expanded_States
                                      .Bare_Document.List.Length > 0)
                                   and Group_Mode = Mode_Break
                                 then
                                   Document
                                     .Bare_Document
                                     .Command
                                     .Expanded_States
                                     .Bare_Document
                                     .List
                                     .Last_Element
                                 else
                                   Document
                                     .Bare_Document
                                     .Command
                                     .Group_Contents);

                           begin
                              Gnatfmt_Trace.Trace ("F15");
                              Fit_Commands.Append
                                (Fit_Command_Type'(Group_Mode, Contents));
                           end;

                        when Command_If_Break =>
                           declare
                              Group_Mode : constant Mode_Kind :=
                                (if Document
                                      .Bare_Document
                                      .Command
                                      .If_Break_Group_Id
                                    /= No_Symbol
                                 then
                                   (if Group_Mode_Map.Contains
                                         (Document
                                            .Bare_Document
                                            .Command
                                            .If_Break_Group_Id)
                                    then
                                      Group_Mode_Map.Element
                                        (Document
                                           .Bare_Document
                                           .Command
                                           .If_Break_Group_Id)
                                    else
                                      Mode_Flat)
                                 else
                                   Mode);
                              Contents : constant Document_Type :=
                                (if Group_Mode = Mode_Break then
                                   Document
                                     .Bare_Document
                                     .Command
                                     .Break_Contents
                                 else
                                   Document
                                     .Bare_Document
                                     .Command
                                 .Flat_Contents);

                           begin
                              Gnatfmt_Trace.Trace ("F16");
                              if Contents /= No_Document
                                and then (if Contents.Bare_Document.Kind in
                                               Document_Text
                                          then
                                            not Contents
                                                  .Bare_Document
                                                  .Text
                                                  .Text
                                                  .Is_Empty)
                              then
                                 Gnatfmt_Trace.Trace ("F17");
                                 Fit_Commands.Append
                                   (Fit_Command_Type'(Mode, Contents));
                              end if;
                           end;

                        when Command_Line =>
                           Gnatfmt_Trace.Trace ("F18");
                           if Mode = Mode_Break
                             or Document.Bare_Document.Command.Hard
                           then
                              Gnatfmt_Trace.Trace ("F19");
                              return True;
                           end if;

                           if not Document.Bare_Document.Command.Soft then
                              Gnatfmt_Trace.Trace ("F20");
                              Append (Current_Line, (" ", 1));
                              Remaining_Width := @ - Natural (1);
                           end if;

                        when Command_Line_Suffix =>
                           Gnatfmt_Trace.Trace ("F21");
                           Current_Has_Line_Suffix := True;

                        when Command_Line_Suffix_Boundary =>
                           Gnatfmt_Trace.Trace ("F22");
                           if Current_Has_Line_Suffix then
                              Gnatfmt_Trace.Trace ("F23");
                              return False;
                           end if;

                        when Command_Break_Parent
                             | Command_Cursor =>
                           null;

                     end case;
               end case;
            end;
         end if;
      end loop;

      return False;
   end Fits;

   ------------
   -- Format --
   ------------

   function Format
     (Document : Document_Type;
      Options  : Format_Options_Type := Default_Format_Options)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use type Ada.Containers.Count_Type;
      use type VSS.Strings.Virtual_String;

      End_Of_Line : constant Prettier_String :=
        (case Options.End_Of_Line is
            when LF =>
              (VSS.Strings.Empty_Virtual_String
               & VSS.Characters.Latin.Line_Feed,
               0),
            when CR =>
              (VSS.Strings.Empty_Virtual_String
               & VSS.Characters.Latin.Carriage_Return,
               0),
            when CRLF =>
              (VSS.Strings.Empty_Virtual_String
               & VSS.Characters.Latin.Carriage_Return
               & VSS.Characters.Latin.Line_Feed,
               0));

      Group_Mode_Map : Symbol_To_Mode_Map;

      Pos : Natural := 0;

      Should_Remeasure : Boolean := False;

      Line_Suffix : Print_Command_Type_Vector := [];

      Print_Commands : Print_Command_Type_Vector :=
        [Print_Command_Type'(Root_Indent, Mode_Break, Document)];

      Printed_Cursor_Count : Natural := 0;

      Result : Prettier_String := Empty_Prettier_String;

   begin

      Propagate_Breaks (Document);

      while Print_Commands.Length > 0 loop
         declare
            Print_Command : constant Print_Command_Type :=
              Print_Commands.Last_Element;
            Indentation   : Indentation_Queue_Type
              renames Print_Command.Indentation;
            Mode          : Mode_Kind renames Print_Command.Mode;
            Document      : Document_Type renames Print_Command.Document;

            procedure Process_Document_Command_Group;
            --  TODO: Description

            procedure Process_Document_Command_Fill;
            --  TODO: Description

            procedure Process_Document_Command_Line;
            --  TODO: Description

            procedure Process_Document_List;
            --  TODO: Description

            procedure Process_Document_Text;
            --  TODO: Description

            ------------------------------------
            -- Process_Document_Command_Group --
            ------------------------------------

            procedure Process_Document_Command_Group is
               procedure Process_Mode_Break;
               --  TODO

               procedure Process_Mode_Flat;
               --  TODO

               ------------------------
               -- Process_Mode_Break --
               ------------------------

               procedure Process_Mode_Break is
                  Next : constant Print_Command_Type :=
                    (Indentation => Indentation,
                     Mode        => Mode_Flat,
                     Document    =>
                       Document
                         .Bare_Document
                         .Command
                         .Group_Contents);
                  Remainder : constant Integer :=
                    Options.Width - Pos;
                  Has_Line_Suffix : constant Boolean :=
                    Line_Suffix.Length > 0;

               begin
                  Gnatfmt_Trace.Trace ("72");
                  Should_Remeasure := False;
                  if not Document.Bare_Document.Command.Break
                     and then
                       Fits
                         (Next,
                          Print_Commands,
                          Remainder,
                          Has_Line_Suffix,
                          Group_Mode_Map)
                  then
                     Gnatfmt_Trace.Trace ("721");
                     Gnatfmt_Trace.Trace
                       ((if Next.Mode = Mode_Flat
                         then "flat"
                         else "break"));
                     Print_Commands.Append (Next);

                  else
                     Gnatfmt_Trace.Trace ("722");
                     if Document
                          .Bare_Document
                          .Command
                          .Expanded_States
                        /= No_Document
                       and then Document
                                  .Bare_Document
                                  .Command
                                  .Expanded_States
                                  .Bare_Document
                                  .List.Length /= 0
                     then
                        Gnatfmt_Trace.Trace ("7221");
                        declare
                           Expanded_States :
                             constant Document_Vector :=
                               Document
                                 .Bare_Document
                                 .Command
                                 .Expanded_States
                                 .Bare_Document
                                 .List;
                           Most_Expanded :
                             constant Document_Type :=
                               Expanded_States.Last_Element;

                        begin
                           if Document
                                .Bare_Document
                                .Command
                                .Break
                           then
                              Gnatfmt_Trace.Trace ("72211");
                              Gnatfmt_Trace.Trace ("break");
                              Print_Commands.Append
                                (Print_Command_Type'
                                   (Indentation,
                                    Mode_Break,
                                    Most_Expanded));

                           else
                              Gnatfmt_Trace.Trace ("72212");
                              for J in
                                 Expanded_States.First_Index + Natural (1)
                                 .. Expanded_States.Last_Index + Natural (1)
                              loop
                                 if J
                                    >= Expanded_States.Last_Index + Natural (1)
                                 then
                                    Gnatfmt_Trace.Trace ("722121");
                                    Gnatfmt_Trace.Trace ("break");
                                    Print_Commands.Append
                                      (Print_Command_Type'
                                         (Indentation,
                                          Mode_Break,
                                          Most_Expanded));
                                 else
                                    Gnatfmt_Trace.Trace ("722122");
                                    declare
                                       State         :
                                         constant Document_Type :=
                                           Expanded_States (J);
                                       Print_Command :
                                         constant
                                           Print_Command_Type :=
                                             (Indentation,
                                              Mode_Flat,
                                              State);
                                    begin
                                       if Fits
                                            (Print_Command,
                                             Print_Commands,
                                             Remainder,
                                             Has_Line_Suffix,
                                             Group_Mode_Map)
                                       then
                                          Gnatfmt_Trace.Trace ("7221221");
                                          Gnatfmt_Trace.Trace
                                            ((if Print_Command.Mode
                                                 = Mode_Flat
                                              then "flat"
                                              else "break"));
                                          Print_Commands.Append
                                            (Print_Command);
                                          exit;
                                       end if;
                                    end;
                                 end if;
                              end loop;
                           end if;
                        end;

                     else
                        Gnatfmt_Trace.Trace ("7222");
                        Gnatfmt_Trace.Trace ("break");
                        Print_Commands.Append
                          (Print_Command_Type'
                             (Indentation,
                              Mode_Break,
                              Document
                                .Bare_Document
                                .Command
                                .Group_Contents));
                     end if;
                  end if;
               end Process_Mode_Break;

               -----------------------
               -- Process_Mode_Flat --
               -----------------------

               procedure Process_Mode_Flat is
                  Print_Command : constant Print_Command_Type :=
                    Print_Command_Type'
                           (Indentation,
                            (if Document
                                  .Bare_Document
                                  .Command
                                  .Break
                             then Mode_Break
                             else Mode_Flat),
                            Document
                              .Bare_Document
                              .Command
                              .Group_Contents);
               begin
                  Gnatfmt_Trace.Trace ("71");
                  if not Should_Remeasure then
                     Gnatfmt_Trace.Trace ("711");
                     Gnatfmt_Trace.Trace
                       ((if Print_Command.Mode = Mode_Flat
                         then "flat"
                         else "break"));
                     Print_Commands.Append (Print_Command);
                  else
                     Gnatfmt_Trace.Trace ("712");
                     Process_Mode_Break;
                  end if;
               end Process_Mode_Flat;

            begin
               case Mode is
                  when Mode_Flat =>
                     Process_Mode_Flat;

                  when Mode_Break =>
                     Process_Mode_Break;

                  when None =>
                     null;
               end case;

               if Document.Bare_Document.Command.Id
                  /= No_Symbol
               then
                  Group_Mode_Map.Include
                    (Document.Bare_Document.Command.Id,
                     Print_Commands.Last_Element.Mode);
               end if;
            end Process_Document_Command_Group;

            -----------------------------------
            -- Process_Document_Command_Fill --
            -----------------------------------

            procedure Process_Document_Command_Fill is
               Remainder : constant Integer := Options.Width - Pos;
               Parts     : constant Document_Vector :=
                 Document
                   .Bare_Document
                   .Command
                   .Parts
                   .Bare_Document
                   .List;

            begin
               if Parts.Length = 1 then
                  declare
                     Content_Flat_Command  : constant Print_Command_Type :=
                         (Indentation, Mode_Flat, Parts.First_Element);
                     Content_Break_Command : constant Print_Command_Type :=
                         (Indentation, Mode_Break, Parts.First_Element);
                     Content_Fits          : constant Boolean :=
                       Fits
                         (Content_Flat_Command,
                          [],
                          Remainder,
                          Line_Suffix.Length > 0,
                          Group_Mode_Map,
                          True);

                  begin
                     if Content_Fits then
                        Print_Commands.Append (Content_Flat_Command);
                     else
                        Print_Commands.Append (Content_Break_Command);
                     end if;
                  end;

               elsif Parts.Length = 2 then
                  declare
                     Content_Flat_Command  : constant Print_Command_Type :=
                        (Indentation, Mode_Flat, Parts.First_Element);
                     Content_Break_Command : constant Print_Command_Type :=
                        (Indentation, Mode_Break, Parts.First_Element);
                     White_Flat_Command    : constant Print_Command_Type :=
                        (Indentation,
                         Mode_Flat,
                         Parts.Element (Parts.First_Index + Natural (1)));
                     White_Break_Command   : constant Print_Command_Type :=
                        (Indentation,
                         Mode_Break,
                         Parts (Parts.First_Index + Natural (1)));
                     Content_Fits : constant Boolean :=
                       Fits
                         (Content_Flat_Command,
                          [],
                          Remainder,
                          Line_Suffix.Length > 0,
                          Group_Mode_Map,
                          True);

                  begin
                     if Content_Fits then
                        Print_Commands.Append (White_Flat_Command);
                        Print_Commands.Append (Content_Flat_Command);

                     else
                        Print_Commands.Append (White_Break_Command);
                        Print_Commands.Append (Content_Break_Command);
                     end if;
                  end;

               elsif Parts.Length /= 0 then
                  declare
                     Content_Flat_Command  :
                       constant Print_Command_Type :=
                         (Indentation, Mode_Flat, Parts.First_Element);
                     Content_Break_Command :
                       constant Print_Command_Type :=
                         (Indentation, Mode_Break, Parts.First_Element);
                     White_Flat_Command    :
                       constant Print_Command_Type :=
                         (Indentation,
                          Mode_Flat,
                          Parts.Element (Parts.First_Index + Natural (1)));
                     White_Break_Command   :
                       constant Print_Command_Type :=
                         (Indentation,
                          Mode_Break,
                          Parts.Element (Parts.First_Index + Natural (1)));

                     Content_Fits : constant Boolean :=
                       Fits
                         (Content_Flat_Command,
                          [],
                          Remainder,
                          Line_Suffix.Length > 0,
                          Group_Mode_Map,
                          True);

                     Remaining_Parts         : constant Document_Vector :=
                       Slice
                         (Parts,
                          Positive (Parts.First_Index + Natural (2)),
                          Positive (Parts.Last_Index));
                     Remaining_Print_Command : constant Print_Command_Type :=
                       (Indentation,
                        Mode,
                        Prettier_Ada.Documents.Builders.Fill
                          (Remaining_Parts));

                     First_And_Second_Content_Flat_Command :
                       constant Print_Command_Type :=
                         (Indentation,
                          Mode_Flat,
                          Prettier_Ada.Documents.Builders.List
                            (Slice
                               (Parts,
                                Positive (Parts.First_Index),
                                Positive (Parts.First_Index + Natural (2)))));
                     First_And_Second_Content_Fits         :
                       constant Boolean :=
                         Fits
                           (First_And_Second_Content_Flat_Command,
                            [],
                            Remainder,
                            Line_Suffix.Length > 0,
                            Group_Mode_Map,
                            True);

                  begin
                     if First_And_Second_Content_Fits then
                        Print_Commands.Append (Remaining_Print_Command);
                        Print_Commands.Append (White_Flat_Command);
                        Print_Commands.Append (Content_Flat_Command);

                     elsif Content_Fits then
                        Print_Commands.Append (Remaining_Print_Command);
                        Print_Commands.Append (White_Break_Command);
                        Print_Commands.Append (Content_Flat_Command);

                     else
                        Print_Commands.Append (Remaining_Print_Command);
                        Print_Commands.Append (White_Break_Command);
                        Print_Commands.Append (Content_Break_Command);
                     end if;
                  end;
               end if;
            end Process_Document_Command_Fill;

            -----------------------------------
            -- Process_Document_Command_Line --
            -----------------------------------

            procedure Process_Document_Command_Line is
               procedure Process_Mode_Break;
               --  TODO

               procedure Process_Mode_Flat;
               --  TODO

               ------------------------
               -- Process_Mode_Break --
               ------------------------

               procedure Process_Mode_Break is
               begin
                  Gnatfmt_Trace.Trace ("131");
                  if not Line_Suffix.Is_Empty then
                     Gnatfmt_Trace.Trace ("1311");
                     Print_Commands.Append
                       (Print_Command_Type'(Indentation, Mode, Document));
                     Gnatfmt_Trace.Trace
                       ("1312 Length" & Line_Suffix.Length'Image);
                     for Suffix of reverse Line_Suffix loop
                        Print_Commands.Append (Suffix);
                     end loop;
                     Line_Suffix.Clear;

                  else
                     Gnatfmt_Trace.Trace ("1312");
                     if Document.Bare_Document.Command.Literal then
                        Gnatfmt_Trace.Trace ("13121");
                        if Indentation.Root /= null then
                           Gnatfmt_Trace.Trace ("131211");
                           Append (Result, End_Of_Line);
                           Append (Result, Indentation.Root.Value);
                           Pos :=
                             Natural (Indentation.Root.Value.Display_Width);

                        else
                           Gnatfmt_Trace.Trace ("131212");
                           Append (Result, End_Of_Line);
                           Pos := 0;
                        end if;

                     else
                        Gnatfmt_Trace.Trace ("13122");
                        Pos := @ - Trim (Result);
                        Append (Result, End_Of_Line);
                        Append (Result, Indentation.Value);
                        Pos := Natural (Indentation.Value.Display_Width);
                     end if;
                  end if;
               end Process_Mode_Break;

               -----------------------
               -- Process_Mode_Flat --
               -----------------------

               procedure Process_Mode_Flat is
               begin
                  Gnatfmt_Trace.Trace ("132");
                  if not Document.Bare_Document.Command.Hard then
                     Gnatfmt_Trace.Trace ("1321");
                     if not Document
                              .Bare_Document
                              .Command
                              .Soft
                     then
                        Gnatfmt_Trace.Trace ("13211");
                        Append (Result, (" ", 1));
                        Pos := @ + Natural (1);
                     end if;

                  else
                     Gnatfmt_Trace.Trace ("1322");
                     Should_Remeasure := True;
                     Process_Mode_Break;
                  end if;
               end Process_Mode_Flat;

            begin
               Gnatfmt_Trace.Trace ("13");
               case Mode is
                  when Mode_Flat =>
                     Process_Mode_Flat;

                  when Mode_Break =>
                     Process_Mode_Break;

                  when None =>
                     null;
               end case;
            end Process_Document_Command_Line;

            ---------------------------
            -- Process_Document_Text --
            ---------------------------

            procedure Process_Document_Text is
            begin
               Gnatfmt_Trace.Trace ("1");
               Append
                 (Result, Document.Bare_Document.Text);
               if Print_Commands.Length > 0 then
                  Pos := @ + Document.Bare_Document.Text.Display_Width;
               end if;
            end Process_Document_Text;

            ---------------------------
            -- Process_Document_List --
            ---------------------------

            procedure Process_Document_List is
               Documents : constant Document_Vector :=
                 Document.Bare_Document.List;

            begin
               Gnatfmt_Trace.Trace ("2");
               Gnatfmt_Trace.Trace ("2 Length" & Documents.Length'Image);
               for Child_Document of reverse Documents loop
                  Ada.Assertions.Assert (Child_Document /= No_Document);
                  Print_Commands.Append
                    (Print_Command_Type'(Indentation, Mode, Child_Document));
               end loop;
            end Process_Document_List;

         begin
            --  Ada.Text_IO.New_Line;
            Print_Commands.Delete_Last;

            case Document.Bare_Document.Kind is
               when Document_Text =>
                  Process_Document_Text;

               when Document_List =>
                  Process_Document_List;

               when Document_Command =>
                  case Document.Bare_Document.Command.Kind is
                     when Command_Cursor =>
                        Gnatfmt_Trace.Trace ("3");
                        if Printed_Cursor_Count >= 2 then
                           --  TODO: Replace this by a GNATfmt defined
                           --  exception
                           raise Program_Error;
                        end if;
                        --  TODO: How to add this command to Result?
                        --  Should Result be instead a Document_Type_Vector
                        --  with only Document_Text and Command_Cursor?
                        Printed_Cursor_Count := @ + Natural (1);

                     when Command_Indent =>
                        Gnatfmt_Trace.Trace ("4");
                        Ada.Assertions.Assert
                          (Document.Bare_Document.Command.Indent_Contents
                           /= No_Document);
                        Print_Commands.Append
                          (Print_Command_Type'
                             (Make_Indentation
                                (Indentation, Options.Indentation),
                              Mode,
                              Document.Bare_Document.Command.Indent_Contents));

                     when Command_Align =>
                        Gnatfmt_Trace.Trace ("5");
                        Ada.Assertions.Assert
                          (Document.Bare_Document.Command.Align_Contents
                           /= No_Document);
                        Print_Commands.Append
                          (Print_Command_Type'
                             (Make_Align
                                (Indentation,
                                 Document.Bare_Document.Command.Align_Data,
                                 Options,
                                 Pos),
                              Mode,
                              Document.Bare_Document.Command.Align_Contents));

                     when Command_Trim =>
                        Gnatfmt_Trace.Trace ("6");
                        Pos := @ - Trim (Result);

                     when Command_Group =>
                        Gnatfmt_Trace.Trace ("7");
                        Process_Document_Command_Group;

                     when Command_Fill =>
                        Gnatfmt_Trace.Trace ("8");
                        Process_Document_Command_Fill;

                     when Command_If_Break =>
                        Gnatfmt_Trace.Trace ("9");
                        declare
                           Group_Mode : constant Mode_Kind :=
                             (if Document
                                   .Bare_Document
                                   .Command
                                   .If_Break_Group_Id
                                 /= No_Symbol
                              then
                                 (if Group_Mode_Map.Contains
                                      (Document
                                         .Bare_Document
                                         .Command
                                         .If_Break_Group_Id)
                                  then
                                    Group_Mode_Map.Element
                                    (Document
                                       .Bare_Document
                                       .Command
                                       .If_Break_Group_Id)
                                  else
                                    None)
                              else
                                 Mode);

                        begin
                           case Group_Mode is
                              when Mode_Break =>
                                 declare
                                    Break_Contents : Document_Type
                                      renames Document
                                                .Bare_Document
                                                .Command
                                                .Break_Contents;

                                 begin
                                    Gnatfmt_Trace.Trace ("91");
                                    if Break_Contents /= No_Document
                                      and then
                                        not (Break_Contents.Bare_Document.Kind
                                             in Document_Text
                                             and then Break_Contents
                                                        .Bare_Document
                                                        .Text
                                                        .Text
                                                        .Is_Empty)
                                    then
                                       Gnatfmt_Trace.Trace ("911");
                                       Print_Commands.Append
                                         (Print_Command_Type'
                                            (Indentation,
                                             Mode,
                                             Break_Contents));
                                    end if;
                                 end;

                              when Mode_Flat =>
                                 declare
                                    Flat_Contents : Document_Type
                                      renames Document
                                                .Bare_Document
                                                .Command
                                          .Flat_Contents;

                                 begin
                                    Gnatfmt_Trace.Trace ("92");
                                    if Flat_Contents /= No_Document
                                      and then
                                        not (Flat_Contents.Bare_Document.Kind
                                             in Document_Text
                                             and then Flat_Contents
                                                        .Bare_Document
                                                        .Text
                                                        .Text
                                                        .Is_Empty)

                                    then
                                       Gnatfmt_Trace.Trace ("921");
                                       Print_Commands.Append
                                         (Print_Command_Type'
                                            (Indentation,
                                             Mode,
                                             Flat_Contents));
                                    end if;
                                 end;

                              when None =>
                                 null;
                           end case;
                        end;

                     when Command_Indent_If_Break  =>
                        Gnatfmt_Trace.Trace ("10");
                        declare
                           Group_Mode : constant Mode_Kind :=
                             (if Document
                                   .Bare_Document
                                   .Command
                                   .Indent_If_Break_Group_Id
                                 /= No_Symbol
                                and then Group_Mode_Map.Contains
                                           (Document
                                              .Bare_Document
                                              .Command
                                              .Indent_If_Break_Group_Id)
                              then
                                 Group_Mode_Map.Element
                                   (Document
                                      .Bare_Document
                                      .Command
                                      .Indent_If_Break_Group_Id)
                              else
                                 Mode);

                        begin
                           case Group_Mode is
                              when Mode_Break =>
                                 declare
                                    Break_Contents : constant Document_Type :=
                                      (if Document.Bare_Document.Command.Negate
                                       then Document
                                              .Bare_Document
                                              .Command
                                              .Indent_If_Break_Contents
                                       else
                                         Prettier_Ada
                                           .Documents
                                           .Builders
                                           .Indent
                                              (Document
                                               .Bare_Document
                                               .Command
                                               .Indent_If_Break_Contents));

                                 begin
                                    Gnatfmt_Trace.Trace ("101");
                                    Gnatfmt_Trace.Trace ("1011");
                                    Print_Commands.Append
                                      (Print_Command_Type'
                                         (Indentation,
                                          Mode,
                                          Break_Contents));
                                 end;

                              when Mode_Flat =>
                                 declare
                                    Flat_Contents : constant Document_Type :=
                                      (if Document.Bare_Document.Command.Negate
                                       then
                                         Prettier_Ada.Documents.Builders.Indent
                                           (Document
                                              .Bare_Document
                                              .Command
                                              .Indent_If_Break_Contents)
                                       else Document
                                              .Bare_Document
                                              .Command
                                              .Indent_If_Break_Contents);

                                 begin
                                    Gnatfmt_Trace.Trace ("102");
                                    Gnatfmt_Trace.Trace ("1021");
                                    Print_Commands.Append
                                      (Print_Command_Type'
                                         (Indentation,
                                          Mode,
                                          Flat_Contents));
                                 end;

                              when None =>
                                 null;
                           end case;
                        end;

                     when Command_Line_Suffix =>
                        Gnatfmt_Trace.Trace ("11");
                        Line_Suffix.Append
                          (Print_Command_Type'
                             (Indentation,
                              Mode,
                              Document
                                .Bare_Document
                                .Command
                                .Line_Suffix_Contents));

                     when Command_Line_Suffix_Boundary =>
                        Gnatfmt_Trace.Trace ("12");
                        if not Line_Suffix.Is_Empty then
                           Gnatfmt_Trace.Trace ("121");
                           Print_Commands.Append
                             (Print_Command_Type'
                                (Indentation,
                                 Mode,
                                 Prettier_Ada
                                   .Documents
                                   .Builders
                                   .Hard_Line_Without_Break_Parent));
                        end if;

                     when Command_Line =>
                        Process_Document_Command_Line;

                     when Command_Label =>
                        Gnatfmt_Trace.Trace ("14");
                        Print_Commands.Append
                          (Print_Command_Type'
                             (Indentation,
                              Mode,
                              Document.Bare_Document.Command.Label_Contents));

                     when Command_Break_Parent =>
                        Gnatfmt_Trace.Trace ("15");
                        null;
                  end case;
            end case;

            if Print_Commands.Length = 0 and Line_Suffix.Length > 0 then
               for Suffix of reverse Line_Suffix loop
                  Print_Commands.Append (Suffix);
               end loop;
               Line_Suffix.Clear;
            end if;
         end;
      end loop;

      Prettier_Ada.Documents.Builders.Reset_Document_Id;

      return VSS.Strings.Conversions.To_Unbounded_UTF_8_String (Result.Text);

   exception
      when others =>
         Prettier_Ada.Documents.Builders.Reset_Document_Id;
         return Ada.Strings.Unbounded.Null_Unbounded_String;
   end Format;

   ----------
   -- Hash --
   ----------

   function Hash (Document : Document_Type) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Document.Bare_Document.Id));

   ----------
   -- Hash --
   ----------

   function Hash (Symbol : Symbol_Type) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Symbol));

   --------------------------
   -- Generate_Indentation --
   --------------------------

   function Generate_Indentation
     (From    : Indentation_Queue_Type;
      Data    : Indentation_Data_Type;
      Options : Indentation_Options_Type)
      return Indentation_Queue_Type
   is
      Value : Prettier_String;
      Queue : Indentation_Data_Vector := From.Queue;

      Last_Tabs   : Natural := 0;
      Last_Spaces : Natural := 0;

      procedure Add_Tabs (Count : Natural);
      --  TODO: Description

      procedure Add_Spaces (Count : Natural);
      --  TODO: Description

      procedure Flush;
      --  TODO: Description

      procedure Flush_Tabs;
      --  TODO: Description

      procedure Flush_Spaces;
      --  TODO: Description

      procedure Reset_Last;
      --  TODO: Description

      --------------
      -- Add_Tabs --
      --------------

      procedure Add_Tabs (Count : Natural) is
      begin
         VSS.Strings.Append
           (Value.Text,
            VSS.Strings."*"
              (VSS.Strings.Character_Count (Count),
               VSS.Characters.Latin.Character_Tabulation));
         Value.Display_Width :=
           @ + VSS.Strings.Display_Cell_Count (Options.Width * Count);
      end Add_Tabs;

      ----------------
      -- Add_Spaces --
      ----------------

      procedure Add_Spaces (Count : Natural)
      is
      begin
         VSS.Strings.Append
           (Value.Text,
            VSS.Strings."*"
              (VSS.Strings.Character_Count (Count),
               VSS.Characters.Latin.Space));
         Value.Display_Width := @ + VSS.Strings.Display_Cell_Count (Count);
      end Add_Spaces;

      -----------
      -- Flush --
      -----------

      procedure Flush
      is
      begin
         case Options.Kind is
            when Tabs => Flush_Tabs;
            when Spaces => Flush_Spaces;
         end case;
      end Flush;

      ----------------
      -- Flush_Tabs --
      ----------------

      procedure Flush_Tabs
      is
      begin
         if Last_Tabs > 0 then
            Add_Tabs (Last_Tabs);
         end if;
         Reset_Last;
      end Flush_Tabs;

      ------------------
      -- Flush_Spaces --
      ------------------

      procedure Flush_Spaces
      is
      begin
         if Last_Spaces > 0 then
            Add_Spaces (Last_Spaces);
         end if;
         Reset_Last;
      end Flush_Spaces;

      ----------------
      -- Reset_Last --
      ----------------

      procedure Reset_Last
      is
      begin
         Last_Tabs := 0;
         Last_Spaces := 0;
      end Reset_Last;

   begin
      if Data.Kind = Dedent then
         Queue.Delete_Last;

      else
         Queue.Append (Data);
      end if;

      for Part of Queue loop
         case Part.Kind is
            when Indent =>
               Flush;
               case Options.Kind is
                  when Tabs => Add_Tabs (1);
                  when Spaces => Add_Spaces (Options.Width);
               end case;

            when String_Align =>
               Flush;
               Append (Value, Part.Text);

            when Number_Align =>
               Last_Tabs := @ + Natural (1);
               Last_Spaces := @ + Part.Width;

            when Inner_Root =>
               --  Last_Tabs and Last_Spaces are not flushed only when
               --  Part.Kind = Number_Align. By simply adjusting Last_Spaces
               --  based on the current line length, any previously unflushed
               --  Last_Tabs and Last_Spaces become irrelevant, and the number
               --  of tabs added when Part.Kind = Indent is kept.
               Last_Spaces := Part.Margin - Value.Display_Width;

            when Dedent =>
               raise Program_Error; -- TODO: Make this a logic error
         end case;
      end loop;

      Flush_Spaces;

      return Indentation_Queue_Type'(Value, Queue, From.Root);
   end Generate_Indentation;

   ------------------------
   -- Get_Document_Parts --
   ------------------------

   function Get_Document_Parts
     (Document : Document_Type)
      return Document_Vector
   is ((case Document.Bare_Document.Kind is
           when Document_List => Document.Bare_Document.List,
           when Document_Command =>
              (case Document.Bare_Document.Command.Kind is
                  when Command_Fill =>
                     Document.Bare_Document.Command.Parts.Bare_Document.List,
                  when others => raise Program_Error),
           when others => raise Program_Error));
   --  TODO: Replace Program_Error by a Gnatfmt defined exception

   ----------------
   -- Make_Align --
   ----------------

   function Make_Align
     (From                : Indentation_Queue_Type;
      Align_Data          : Alignment_Data_Type;
      Options             : Format_Options_Type;
      Current_Line_Length : Natural)
      return Indentation_Queue_Type
   is

   begin
      case Align_Data.Kind is
         when None =>
            return From;

         when Width =>
            return
              Generate_Indentation
                (From,
                 Indentation_Data_Type'(Number_Align, Align_Data.N),
                 Options.Indentation);

         when Text =>
            return
              Generate_Indentation
                (From,
                 Indentation_Data_Type'
                   (String_Align,
                    (declare
                       Text : constant VSS.Strings.Virtual_String :=
                         VSS.Strings.Conversions.To_Virtual_String
                           (Align_Data.T);
                     begin
                       (Text, VSS.Strings.Utilities.Display_Width (Text)))),
                 Options.Indentation);

         when Dedent_To_Root =>
            if From.Root = null then
               return Root_Indent;
            end if;

            return From.Root.all;

         when Dedent =>
            return
              Generate_Indentation
                (From,
                 Indentation_Data_Type'(Kind => Dedent),
                 Options.Indentation);

         when Inner_Root =>
            return
              Generate_Indentation
                (From,
                 Indentation_Data_Type'
                   (Kind => Inner_Root, Margin => Current_Line_Length),
                 Options.Indentation);

         when Root =>
            declare
               Result : Indentation_Queue_Type := From;

            begin
               Result.Root := new Indentation_Queue_Type'(From);
               return Result;
            end;
      end case;

   end Make_Align;

   ----------------------
   -- Make_Indentation --
   ----------------------

   function Make_Indentation
     (From    : Indentation_Queue_Type;
      Options : Indentation_Options_Type)
      return Indentation_Queue_Type
   is (Generate_Indentation
         (From, Indentation_Data_Type'(Kind => Indent), Options));

   ---------------------
   -- New_Document_Id --
   ---------------------

   function New_Document_Id return Natural is
   begin
      return Result : constant Natural := Document_Id do
         Document_Id := @ + Natural (1);
      end return;
   end New_Document_Id;

   ----------------
   -- New_Symbol --
   ----------------

   function New_Symbol return Symbol_Type is
   begin
      return S : constant Symbol_Type := Current_Symbol do
         Current_Symbol := Current_Symbol + 1;
      end return;
   end New_Symbol;

   ----------------------
   -- Propagate_Breaks --
   ----------------------

   procedure Propagate_Breaks (Document : Document_Type) is
      Already_Visited : Document_Hashed_Set;
      Group_Stack     : Document_Vector;

      function Propagate_Breaks_On_Enter
        (Document : Document_Type)
         return Optional_Boolean;
      --  TODO: Description

      function Propagate_Breaks_On_Exit
        (Document : Document_Type)
         return Optional_Boolean;
      --  TODO: Description

      -------------------------------
      -- Propagate_Breaks_On_Enter --
      -------------------------------

      function Propagate_Breaks_On_Enter
        (Document : Document_Type)
         return Optional_Boolean
      is

      begin
         Gnatfmt_Trace.Trace ("PonEnter");
         Gnatfmt_Trace.Trace (Already_Visited.Length'Image);
         if Document = No_Document then
            return Optional_Boolean'(Is_Set => False);
         end if;

         case Document.Bare_Document.Kind is
            when Document_Command =>
               case Document.Bare_Document.Command.Kind is
                  when Command_Break_Parent =>
                     Gnatfmt_Trace.Trace ("PonEnter2");
                     Break_Parent_Group (Group_Stack);

                  when Command_Group =>
                     Gnatfmt_Trace.Trace ("PonEnter3");
                     Group_Stack.Append (Document);
                     if Already_Visited.Contains (Document) then
                        Gnatfmt_Trace.Trace ("PonEnter4");
                        return
                          Optional_Boolean'
                            (Is_Set => True, Value => False);
                     end if;
                     Gnatfmt_Trace.Trace ("PInsert");
                     Already_Visited.Insert (Document);

                  when others =>
                     null;
               end case;

            when others =>
               null;
         end case;

         return Optional_Boolean'(Is_Set => False);
      end Propagate_Breaks_On_Enter;

      ------------------------------
      -- Propagate_Breaks_On_Exit --
      ------------------------------

      function Propagate_Breaks_On_Exit
        (Document : Document_Type)
         return Optional_Boolean
      is
      begin
         Gnatfmt_Trace.Trace ("PonExit");
         if Document = No_Document then
            return Optional_Boolean'(Is_Set => False);
         end if;

         case Document.Bare_Document.Kind is
            when Document_Command =>
               if Document.Bare_Document.Command.Kind in Command_Group then
                  Gnatfmt_Trace.Trace ("PonExit1");
                  declare
                     Group : constant Document_Type :=
                       Group_Stack.Last_Element;

                  begin
                     Group_Stack.Delete_Last;
                     if Group.Bare_Document.Command.Break then
                        Gnatfmt_Trace.Trace ("PonExit2");
                        Break_Parent_Group (Group_Stack);
                     end if;
                  end;
               end if;

            when others =>
               null;
         end case;

         return Optional_Boolean'(Is_Set => False);
      end Propagate_Breaks_On_Exit;

   begin
      Traverse_Document
        (Document                           => Document,
         On_Enter                           =>
           Propagate_Breaks_On_Enter'Access,
         On_Exit                            => Propagate_Breaks_On_Exit'Access,
         Should_Traverse_Conditional_Groups => True);
   end Propagate_Breaks;

   -----------------------
   -- Reset_Document_Id --
   -----------------------

   procedure Reset_Document_Id is
   begin
      Document_Id := Natural'First;
   end Reset_Document_Id;

   -----------------
   -- Root_Indent --
   -----------------

   function Root_Indent return Indentation_Queue_Type is
      (Indentation_Queue_Type'
         (Value  => Empty_Prettier_String,
          Queue  => [],
          Root   => null));

   -----------
   -- Slice --
   -----------

   function Slice
     (Documents : Document_Vector;
      First     : Positive;
      Last      : Positive)
      return Document_Vector is
   begin
      return Result : Document_Vector do
         Result.Reserve_Capacity
           (Ada.Containers.Count_Type (Last - First + Natural (1)));
         for J in First .. Last loop
            Result.Append (Documents.Element (J));
         end loop;
      end return;
   end Slice;

   ----------------------
   -- To_Document_Type --
   ----------------------

   function To_Document_Type (Text : Wide_Wide_String) return Document_Type
   is
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind => Document_Text,
           Text =>
             (declare
                Text_VSS : constant VSS.Strings.Virtual_String :=
                  VSS.Strings.To_Virtual_String (Text);
              begin
                (Text_VSS, VSS.Strings.Utilities.Display_Width (Text_VSS))),
           Id   => New_Document_Id);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end To_Document_Type;

   -----------------------
   -- Traverse_Document --
   -----------------------

   procedure Traverse_Document
     (Document                           : Document_Type;
      On_Enter                           :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      On_Exit                            :
        access function
          (Document : Document_Type)
           return Optional_Boolean := null;
      Should_Traverse_Conditional_Groups : Boolean := False)
   is
      Doc_Stack : Document_Vector := [Document];

      Traverse_Doc_On_Exit_Stack_Marker : constant Document_Type :=
        Prettier_Ada.Documents.Builders.Text
          (Ada.Strings.Unbounded.Null_Unbounded_String);

      use type Ada.Containers.Count_Type;

   begin
      while Doc_Stack.Length > 0 loop
         Gnatfmt_Trace.Trace ("TLen" & Doc_Stack.Length'Image);
         declare
            Doc : constant Document_Type := Doc_Stack.Last_Element;

         begin
            Doc_Stack.Delete_Last;

            if Doc = Traverse_Doc_On_Exit_Stack_Marker then
               Gnatfmt_Trace.Trace ("T1");
               declare
                  Doc : constant Document_Type := Doc_Stack.Last_Element;
                  Ignore : constant Optional_Boolean := On_Exit (Doc);

               begin
                  Doc_Stack.Delete_Last;

                  goto Continue;
               end;
            end if;

            if On_Exit /= null then
               Gnatfmt_Trace.Trace ("T2");
               Doc_Stack.Append (Doc);
               Doc_Stack.Append (Traverse_Doc_On_Exit_Stack_Marker);
            end if;

            if On_Enter /= null then
               declare
                  On_Enter_Result : constant Optional_Boolean :=
                    On_Enter (Doc);

               begin
                  if On_Enter_Result.Is_Set
                    and then not On_Enter_Result.Value
                  then
                     Gnatfmt_Trace.Trace ("T4");
                     goto Continue;
                  end if;
               end;
            end if;

            case Doc.Bare_Document.Kind is
               when Document_Text =>
                  Gnatfmt_Trace.Trace ("T10");
                  null;

               when Document_List =>
                  Gnatfmt_Trace.Trace ("T5");
                  declare
                     Documents : constant Document_Vector :=
                       Doc.Bare_Document.List;

                  begin
                     for Child_Doc of reverse Documents loop
                        Ada.Assertions.Assert (Child_Doc /= No_Document);
                        Doc_Stack.Append (Child_Doc);
                     end loop;
                  end;

               when Document_Command =>
                  case Doc.Bare_Document.Command.Kind is
                     when Command_Align =>
                        Gnatfmt_Trace.Trace ("T9");
                        Ada.Assertions.Assert
                          (Doc.Bare_Document.Command.Align_Contents
                           /= No_Document);
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Align_Contents);

                     when Command_Indent =>
                        Gnatfmt_Trace.Trace ("T9");
                        Ada.Assertions.Assert
                          (Doc.Bare_Document.Command.Indent_Contents
                           /= No_Document);
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Indent_Contents);

                     when Command_Indent_If_Break =>
                        Gnatfmt_Trace.Trace ("T9");
                        Ada.Assertions.Assert
                          (Doc.Bare_Document.Command.Indent_If_Break_Contents
                           /= No_Document);
                        Doc_Stack.Append
                          (Doc
                             .Bare_Document
                             .Command
                             .Indent_If_Break_Contents);

                     when Command_Label =>
                        Gnatfmt_Trace.Trace ("T9");
                        Ada.Assertions.Assert
                          (Doc.Bare_Document.Command.Label_Contents
                           /= No_Document);
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Label_Contents);

                     when Command_Line_Suffix =>
                        Gnatfmt_Trace.Trace ("T9");
                        Ada.Assertions.Assert
                          (Doc.Bare_Document.Command.Line_Suffix_Contents
                           /= No_Document);
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Line_Suffix_Contents);

                     when Command_Fill =>
                        Gnatfmt_Trace.Trace ("T5");
                        Ada.Assertions.Assert
                          (Doc.Bare_Document.Command.Parts.Bare_Document
                             /= null
                           and then Doc
                                      .Bare_Document
                                      .Command.Parts
                                      .Bare_Document
                                      .Kind = Document_List);
                        declare
                           Parts : constant Document_Vector :=
                             Doc
                               .Bare_Document
                               .Command
                               .Parts
                               .Bare_Document
                               .List;
                        begin
                           for Part of reverse Parts loop
                              Ada.Assertions.Assert (Part /= No_Document);
                              Doc_Stack.Append (Part);
                           end loop;
                        end;

                     when Command_Group =>
                        Gnatfmt_Trace.Trace ("T8");
                        if Should_Traverse_Conditional_Groups
                          and Doc
                                .Bare_Document
                                .Command
                                .Expanded_States
                              /= No_Document
                        then
                           Gnatfmt_Trace.Trace ("T11");
                           Ada.Assertions.Assert
                             (Doc
                                .Bare_Document
                                .Command
                                .Expanded_States
                                .Bare_Document
                                .Kind
                              = Document_List);
                           declare
                              Expanded_States : constant Document_Vector :=
                                Doc
                                  .Bare_Document
                                  .Command
                                  .Expanded_States
                                  .Bare_Document
                                  .List;

                           begin
                              for State of reverse Expanded_States loop
                                 Gnatfmt_Trace.Trace ("T13");
                                 Ada.Assertions.Assert (State /= No_Document);
                                 Doc_Stack.Append (State);
                              end loop;
                           end;

                        else
                           Gnatfmt_Trace.Trace ("T14");
                           Ada.Assertions.Assert
                             (Doc.Bare_Document.Command.Group_Contents
                              /= No_Document);
                           Doc_Stack.Append
                             (Doc.Bare_Document.Command.Group_Contents);
                        end if;

                     when Command_If_Break =>
                        Gnatfmt_Trace.Trace ("T7");
                        Ada.Assertions.Assert
                          (Doc.Bare_Document.Command.Flat_Contents
                           /= No_Document);
                        Ada.Assertions.Assert
                          (Doc.Bare_Document.Command.Break_Contents
                           /= No_Document);
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Flat_Contents);
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Break_Contents);

                     when Command_Break_Parent
                          | Command_Cursor
                          | Command_Line
                          | Command_Line_Suffix_Boundary
                          | Command_Trim =>
                        Gnatfmt_Trace.Trace ("T10");
                        null;

                  end case;

            end case;
         end;
         <<Continue>>
      end loop;
   end Traverse_Document;

   ----------
   -- Trim --
   ----------

   procedure Trim
     (Item : in out Prettier_String;
      Side : Trim_End)
   is
      MF : VSS.Strings.Markers.Character_Marker;
      ML : VSS.Strings.Markers.Character_Marker;

      Trim_Count : VSS.Strings.Display_Cell_Count := 0;

   begin
      if Side in Left | Both then
         declare
            J : VSS.Strings.Character_Iterators.Character_Iterator :=
              Item.Text.Before_First_Character;
            C : VSS.Characters.Virtual_Character'Base;

         begin
            while J.Forward (C) loop
               exit when C not in ' '
                                  | VSS.Characters.Latin.Character_Tabulation;

               Trim_Count := @ + 1;
            end loop;

            if J.Has_Element then
               MF := J.Marker;

            else
               Item.Text.Clear;
               Item.Display_Width := 0;

               return;
            end if;
         end;

      else
         MF := Item.Text.At_First_Character.Marker;
      end if;

      if Side in Right | Both then
         declare
            J : VSS.Strings.Character_Iterators.Character_Iterator :=
              Item.Text.After_Last_Character;
            C : VSS.Characters.Virtual_Character'Base;

         begin
            while J.Backward loop
               C := J.Element;

               exit when C not in ' '
                                  | VSS.Characters.Latin.Character_Tabulation;

               Trim_Count := @ + 1;
            end loop;

            if J.Has_Element then
               ML := J.Marker;

            else
               Item.Text.Clear;
               Item.Display_Width := 0;

               return;
            end if;
         end;

      else
         ML := Item.Text.At_Last_Character.Marker;
      end if;

      Item.Text := Item.Text.Slice (MF, ML);
      Item.Display_Width := @ - Trim_Count;
   end Trim;

   ----------
   -- Trim --
   ----------

   function Trim
     (Text : in out Prettier_String)
      return Natural
   is
      Initial_Display_Width : constant VSS.Strings.Display_Cell_Count :=
        Text.Display_Width;

   begin
      Trim (Text, Right);

      return Natural (Initial_Display_Width - Text.Display_Width);
   end Trim;

end Prettier_Ada.Documents.Implementation;
