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

   type Format_State_Record is record
      Current_Line_Length  : Natural;
      Group_Mode_Map       : Symbol_To_Mode_Map;
      Line_Suffix          : Print_Command_Type_Vector;
      Print_Commands       : Print_Command_Type_Vector;
      Printed_Cursor_Count : Natural;
      Result               : Prettier_String;
      Should_Remeasure     : Boolean;
      Last_Was_Hardline    : Boolean;
      --  Flag indicating if the last text added to Result was a hardline
   end record;

   Default_Format_State : constant Format_State_Record :=
     (Current_Line_Length  => 0,
      Group_Mode_Map       => Symbol_To_Mode_Maps.Empty_Map,
      Line_Suffix          => [],
      Print_Commands       => [],
      Printed_Cursor_Count => 0,
      Result               => Empty_Prettier_String,
      Should_Remeasure     => False,
      Last_Was_Hardline    => False);

   procedure Append (To : in out Prettier_String; Source : Prettier_String);
   --  Append Source to the end of To

   procedure Break_Parent_Group
     (Group_Stack : in out Document_Vector);
   --  TODO: Description

   procedure Break_Parent_Table
     (Table_Stack : in out Document_Vector);
   --  Sets Alignment_Table_Must_Break and Break_Parents to True for the last
   --  element of Table_Stack.

   function Fits
     (Next            : Print_Command_Type;
      Rest_Commands   : Print_Command_Type_Vector;
      Width           : Integer;
      Has_Line_Suffix : Boolean;
      Group_Mode_Map  : Symbol_To_Mode_Map;
      Must_Be_Flat    : Boolean := False)
      return Boolean;
   --  TODO: Description

   procedure Format
     (Format_State   : in out Format_State_Record;
      Format_Options : Format_Options_Type);
   --  Resume the formatting operations on Format_State.
   --  Formats according to Format_Options.

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

   function Root_Indent
     (Options : Indentation_Options_Type)
      return Indentation_Queue_Type;
   --  Creates an initial indentation queue with Options.Offset as indentation
   --  offset.

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
               Group_Command.all.Break := True;
            end if;
         end;
      end if;
   end Break_Parent_Group;

   ------------------------
   -- Break_Parent_Table --
   ------------------------

   procedure Break_Parent_Table
     (Table_Stack : in out Document_Vector) is
   begin
      if not Table_Stack.Is_Empty then
         Table_Stack
           .Last_Element
           .Bare_Document
           .Command
           .Alignment_Table_Must_Break := True;
         Table_Stack
           .Last_Element
           .Bare_Document
           .Command
           .Break_Parents := True;
      end if;
   end Break_Parent_Table;

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
      if Remaining_Width = Integer'Last then
         return True;
      end if;

      while Remaining_Width >= 0 loop
         if Fit_Commands.Is_Empty then
            if Rest_Idx < Rest_Commands.First_Index then
               return True;
            end if;

            Fit_Commands.Append (+Rest_Commands.Element (Rest_Idx));
            Rest_Idx := @ - Natural (1);

         else
            declare
               Current_Fit_Command : constant Fit_Command_Type :=
                  Fit_Commands.Last_Element;
               Mode                : Mode_Kind
                 renames Current_Fit_Command.Mode;
               Document            : Document_Type
                 renames Current_Fit_Command.Document;

               use type Ada.Containers.Count_Type;

            begin
               Fit_Commands.Delete_Last;

               --  If No_Document, skip it

               if Document = No_Document then
                  goto Continue;
               end if;

               case Document.Bare_Document.Kind is
                  when Document_Text =>
                     Append
                       (Current_Line, Document.Bare_Document.Text);
                     Remaining_Width :=
                       @ - Document.Bare_Document.Text.Display_Width;

                  when Document_List =>
                     for Child_Document of
                        reverse Get_Document_Parts (Document)
                     loop
                        Fit_Commands.Append
                          (Fit_Command_Type'(Mode, Child_Document));
                     end loop;

                  when Document_Command =>
                     case Document.Bare_Document.Command.Kind is
                        when Command_Fill =>
                           for Child_Document of
                              reverse Get_Document_Parts (Document)
                           loop
                              Fit_Commands.Append
                                (Fit_Command_Type'(Mode, Child_Document));
                           end loop;

                        when Command_Indent =>
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Indent_Contents));

                        when Command_Align =>
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Align_Contents));

                        when Command_Indent_If_Break =>
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Indent_If_Break_Contents));

                        when Command_Label =>
                           Fit_Commands.Append
                             (Fit_Command_Type'
                                (Mode,
                                 Document
                                   .Bare_Document
                                   .Command
                                   .Label_Contents));

                        when Command_Trim =>
                           Remaining_Width := @ + Trim (Current_Line);

                        when Command_Group =>
                           if Must_Be_Flat
                             and Document.Bare_Document.Command.Break
                           then
                              return False;
                           end if;

                           declare
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
                                 Fit_Commands.Append
                                   (Fit_Command_Type'(Mode, Contents));
                              end if;
                           end;

                        when Command_Line =>
                           if Mode = Mode_Break
                             or Document.Bare_Document.Command.Hard
                           then
                              return True;
                           end if;

                           if not Document.Bare_Document.Command.Soft then
                              Append (Current_Line, (" ", 1));
                              Remaining_Width := @ - Natural (1);
                           end if;

                        when Command_Line_Suffix =>
                           Current_Has_Line_Suffix := True;

                        when Command_Line_Suffix_Boundary =>
                           if Current_Has_Line_Suffix then
                              return False;
                           end if;

                        when Command_Break_Parent
                             | Command_Cursor
                             | Command_Alignment_Table_End
                        =>
                           null;

                        when Command_Alignment_Table =>
                           --  First check if the table has more than 1 row and
                           --  must break. In such case, no calculations are
                           --  needed.

                           if Document
                                .Bare_Document
                                .Command
                                .Alignment_Table_Elements
                                .Length > 1
                             and then Document
                                        .Bare_Document
                                        .Command
                                        .Alignment_Table_Must_Break
                           then
                              return False;
                           end if;

                           declare
                              Alignment_Table_Elements   : Document_Table
                                renames Document
                                          .Bare_Document
                                          .Command
                                          .Alignment_Table_Elements;
                              Alignment_Table_Separators : Document_Table
                                renames Document
                                          .Bare_Document
                                          .Command
                                          .Alignment_Table_Separators;

                              --  The algorithm relies on not only checking the
                              --  previous elements/separators of the row but
                              --  also of other rows. The easist way to do this
                              --  is with indices. Start by finding the longest
                              --  column and row.

                              First_Row_Index    : constant Positive :=
                                Alignment_Table_Elements.First_Index;
                              Last_Row_Index     : constant Positive :=
                                Alignment_Table_Elements.Last_Index;
                              First_Column_Index : constant Positive :=
                                Alignment_Table_Elements
                                  .Constant_Reference (First_Row_Index)
                                  .First_Index;
                              Last_Column_Index  : constant Positive :=
                                [for Row_Elements of Alignment_Table_Elements
                                 => Row_Elements.Last_Index]'Reduce
                                                               (Positive'Max,
                                                                1);

                           begin
                              --  Append to Fit_Comands every element/separator
                              --  in reverse order (starting from the last
                              --  column of the last row).

                              for Column_Index
                                in reverse First_Column_Index
                                           .. Last_Column_Index
                              loop
                                 if Alignment_Table_Separators
                                      .Constant_Reference (Last_Row_Index)
                                      .Last_Index
                                    >= Column_Index
                                 then
                                    Fit_Commands.Append
                                      (Fit_Command_Type'
                                         (Mode_Flat,
                                          Alignment_Table_Separators
                                            .Constant_Reference
                                               (Last_Row_Index)
                                            .Constant_Reference
                                               (Column_Index)));
                                 end if;

                                 if Alignment_Table_Elements
                                      .Constant_Reference (Last_Row_Index)
                                      .Last_Index
                                    >= Column_Index
                                 then
                                    Fit_Commands.Append
                                      (Fit_Command_Type'
                                         (Mode_Flat,
                                          Alignment_Table_Elements
                                            .Constant_Reference
                                               (Last_Row_Index)
                                            .Constant_Reference
                                               (Column_Index)));
                                 end if;
                              end loop;

                              for Row_Index
                                in reverse First_Row_Index
                                           .. Standard."-" (Last_Row_Index, 1)
                              loop
                                 --   At a Line between rows

                                 Fit_Commands.Append
                                   (Fit_Command_Type'
                                      (Mode_Flat,
                                       Prettier_Ada.Documents.Builders.Line));

                                 for Column_Index
                                   in reverse First_Column_Index
                                              .. Last_Column_Index
                                 loop
                                    if Alignment_Table_Separators
                                         .Constant_Reference (Row_Index)
                                         .Last_Index
                                       >= Column_Index
                                    then
                                       Fit_Commands.Append
                                         (Fit_Command_Type'
                                            (Mode_Flat,
                                             Alignment_Table_Separators
                                               .Constant_Reference
                                                  (Row_Index)
                                               .Constant_Reference
                                                  (Column_Index)));
                                    end if;

                                    if Alignment_Table_Elements
                                         .Constant_Reference (Row_Index)
                                         .Last_Index
                                       >= Column_Index
                                    then
                                       Fit_Commands.Append
                                         (Fit_Command_Type'
                                            (Mode_Flat,
                                             Alignment_Table_Elements
                                               .Constant_Reference
                                                  (Row_Index)
                                               .Constant_Reference
                                                  (Column_Index)));
                                    end if;
                                 end loop;
                              end loop;
                           end;

                        when Command_Alignment_Table_Separator =>
                           Append
                             (Current_Line,
                              Document
                                .Bare_Document
                                .Command
                                .Alignment_Table_Separator_Text);
                           Remaining_Width :=
                             @
                             - Document
                                 .Bare_Document
                                 .Command
                                 .Alignment_Table_Separator_Text
                                 .Display_Width;

                     end case;
               end case;

               <<Continue>>
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
      State : Format_State_Record := Default_Format_State;

   begin
      --  Adapt State, which at this point is a Default_Format_state, based
      --  on Document (to be formatted) and Options.

      State.Current_Line_Length :=
        Options.Indentation.Offset.Spaces
        + Options.Indentation.Offset.Tabs * Options.Indentation.Width;

      State.Result :=
        (VSS.Strings."&"
           (VSS.Strings."*"
              (VSS.Strings.Character_Count (Options.Indentation.Offset.Tabs),
               VSS.Characters.Latin.Character_Tabulation),
            VSS.Strings."*"
              (VSS.Strings.Character_Count (Options.Indentation.Offset.Spaces),
               VSS.Characters.Latin.Space)),
         VSS.Strings.Display_Cell_Count
           (Options.Indentation.Offset.Spaces
            + Options.Indentation.Offset.Tabs * Options.Indentation.Width));

      State.Print_Commands :=
        [Print_Command_Type'
           (Root_Indent (Options.Indentation),
            Mode_Break,
            Document)];

      Propagate_Breaks (Document);

      Format (State, Options);

      return
        VSS.Strings.Conversions.To_Unbounded_UTF_8_String (State.Result.Text);
   end Format;

   ------------
   -- Format --
   ------------

   procedure Format
     (Format_State   : in out Format_State_Record;
      Format_Options : Format_Options_Type)
   is
      use type Ada.Containers.Count_Type;
      use type VSS.Strings.Virtual_String;

      End_Of_Line : constant Prettier_String :=
        (case Format_Options.End_Of_Line is
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

   begin
      while Format_State.Print_Commands.Length > 0 loop
         declare
            Print_Command : constant Print_Command_Type :=
              Format_State.Print_Commands.Last_Element;
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

            procedure Process_Document_Command_Alignment_Table;
            --  Processes Print_Command as an Alignment_Table.
            --  An Alignment_Table can be processed in two modes:
            --  - Break
            --  - Flat
            --
            --  To process an Alignment Table in Break mode, each column is
            --  handled sequentially. For each column, the required alignment
            --  is calculated and applied. Once a column is processed, its
            --  separators are appended. This process is repeated for each
            --  column until the entire table is processed.
            --
            --  To process an Alignment_Table in Flat mode, each row is handled
            --  sequentually as list.

            procedure Process_Document_Command_Alignment_Table_Separator;
            --  Processes Print_Command as an Alignment_Table_Separator
            --  command. An Alignment_Table_Separator command is only processed
            --  this way if it's not a child document of an Alignment_Table or
            --  if the Alignment_Table was handled in Flat mode. In this case,
            --  the separators' text is simply appended to
            --  Format_State.Result.

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
                  Remaining_Line_Length : constant Integer :=
                    Format_Options.Width - Format_State.Current_Line_Length;
                  Has_Line_Suffix       : constant Boolean :=
                    Format_State.Line_Suffix.Length > 0;

               begin
                  Format_State.Should_Remeasure := False;
                  if not Document.Bare_Document.Command.Break
                     and then
                       Fits
                         (Next,
                          Format_State.Print_Commands,
                          Remaining_Line_Length,
                          Has_Line_Suffix,
                          Format_State.Group_Mode_Map)
                  then
                     Format_State.Print_Commands.Append (Next);

                  else
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
                              Format_State.Print_Commands.Append
                                (Print_Command_Type'
                                   (Indentation,
                                    Mode_Break,
                                    Most_Expanded));

                           else
                              for J in
                                 Expanded_States.First_Index + Natural (1)
                                 .. Expanded_States.Last_Index + Natural (1)
                              loop
                                 if J
                                    >= Expanded_States.Last_Index + Natural (1)
                                 then
                                    Format_State.Print_Commands.Append
                                      (Print_Command_Type'
                                         (Indentation,
                                          Mode_Break,
                                          Most_Expanded));
                                 else
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
                                             Format_State.Print_Commands,
                                             Remaining_Line_Length,
                                             Has_Line_Suffix,
                                             Format_State.Group_Mode_Map)
                                       then
                                          Format_State.Print_Commands.Append
                                            (Print_Command);
                                          exit;
                                       end if;
                                    end;
                                 end if;
                              end loop;
                           end if;
                        end;

                     else
                        Format_State.Print_Commands.Append
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
                  if not Format_State.Should_Remeasure then
                     Format_State.Print_Commands.Append (Print_Command);
                  else
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
                  Format_State.Group_Mode_Map.Include
                    (Document.Bare_Document.Command.Id,
                     Format_State.Print_Commands.Last_Element.Mode);
               end if;
            end Process_Document_Command_Group;

            -----------------------------------
            -- Process_Document_Command_Fill --
            -----------------------------------

            procedure Process_Document_Command_Fill is
               Remaining_Line_Length : constant Integer :=
                 Format_Options.Width - Format_State.Current_Line_Length;
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
                          Remaining_Line_Length,
                          Format_State.Line_Suffix.Length > 0,
                          Format_State.Group_Mode_Map,
                          True);

                  begin
                     if Content_Fits then
                        Format_State
                          .Print_Commands
                          .Append (Content_Flat_Command);

                     else
                        Format_State
                          .Print_Commands
                          .Append (Content_Break_Command);
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
                          Remaining_Line_Length,
                          Format_State.Line_Suffix.Length > 0,
                          Format_State.Group_Mode_Map,
                          True);

                  begin
                     if Content_Fits then
                        Format_State
                          .Print_Commands
                          .Append (White_Flat_Command);
                        Format_State
                          .Print_Commands
                          .Append (Content_Flat_Command);

                     else
                        Format_State
                          .Print_Commands
                          .Append (White_Break_Command);
                        Format_State
                          .Print_Commands
                          .Append (Content_Break_Command);
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
                          Remaining_Line_Length,
                          Format_State.Line_Suffix.Length > 0,
                          Format_State.Group_Mode_Map,
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
                            Remaining_Line_Length,
                            Format_State.Line_Suffix.Length > 0,
                            Format_State.Group_Mode_Map,
                            True);

                  begin
                     if First_And_Second_Content_Fits then
                        Format_State
                          .Print_Commands
                          .Append (Remaining_Print_Command);
                        Format_State
                          .Print_Commands
                          .Append (White_Flat_Command);
                        Format_State
                          .Print_Commands
                          .Append (Content_Flat_Command);

                     elsif Content_Fits then
                        Format_State
                          .Print_Commands
                          .Append (Remaining_Print_Command);
                        Format_State
                          .Print_Commands
                          .Append (White_Break_Command);
                        Format_State
                          .Print_Commands
                          .Append (Content_Flat_Command);

                     else
                        Format_State
                          .Print_Commands
                          .Append (Remaining_Print_Command);
                        Format_State
                          .Print_Commands
                          .Append (White_Break_Command);
                        Format_State
                          .Print_Commands
                          .Append (Content_Break_Command);
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
                  if not Format_State.Line_Suffix.Is_Empty then
                     Format_State.Print_Commands.Append
                       (Print_Command_Type'(Indentation, Mode, Document));
                     for Suffix of reverse Format_State.Line_Suffix loop
                        Format_State.Print_Commands.Append (Suffix);
                     end loop;
                     Format_State.Line_Suffix.Clear;
                     Format_State.Last_Was_Hardline := False;

                  else
                     if Document.Bare_Document.Command.Literal then
                        if Indentation.Root /= null then
                           Append (Format_State.Result, End_Of_Line);
                           Append
                             (Format_State.Result, Indentation.Root.Value);
                           Format_State.Current_Line_Length :=
                             Natural (Indentation.Root.Value.Display_Width);

                        else
                           Append (Format_State.Result, End_Of_Line);
                           Format_State.Current_Line_Length :=
                             Format_Options.Indentation.Offset.Spaces
                             + Format_Options.Indentation.Offset.Tabs
                               * Format_Options.Indentation.Width;
                        end if;
                        Format_State.Last_Was_Hardline := False;

                     else
                        Format_State.Current_Line_Length :=
                          @ - Trim (Format_State.Result);
                        Append (Format_State.Result, End_Of_Line);
                        Append (Format_State.Result, Indentation.Value);
                        Format_State.Current_Line_Length :=
                          Natural (Indentation.Value.Display_Width);
                        Format_State.Last_Was_Hardline := True;
                     end if;
                  end if;
               end Process_Mode_Break;

               -----------------------
               -- Process_Mode_Flat --
               -----------------------

               procedure Process_Mode_Flat is
               begin
                  if not Document.Bare_Document.Command.Hard then
                     if not Document
                              .Bare_Document
                              .Command
                              .Soft
                     then
                        Append (Format_State.Result, (" ", 1));
                        Format_State.Current_Line_Length := @ + Natural (1);
                     end if;
                     Format_State.Last_Was_Hardline := False;

                  else
                     Format_State.Should_Remeasure := True;
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
            end Process_Document_Command_Line;

            ---------------------------
            -- Process_Document_Text --
            ---------------------------

            procedure Process_Document_Text is
            begin
               Append (Format_State.Result, Document.Bare_Document.Text);
               Format_State.Current_Line_Length :=
                 @ + Document.Bare_Document.Text.Display_Width;
            end Process_Document_Text;

            ---------------------------
            -- Process_Document_List --
            ---------------------------

            procedure Process_Document_List is
               Documents : constant Document_Vector :=
                 Document.Bare_Document.List;

            begin
               for Child_Document of reverse Documents loop
                  Format_State.Print_Commands.Append
                    (Print_Command_Type'(Indentation, Mode, Child_Document));
               end loop;
            end Process_Document_List;

            ----------------------------------------------
            -- Process_Document_Command_Alignment_Table --
            ----------------------------------------------

            procedure Process_Document_Command_Alignment_Table
            is
               procedure Process_Mode_Break;
               --  Processes the Alignment_Table comamnd in Break mode

               procedure Process_Mode_Flat;
               --  Processes the Alignment_Table comamnd in Flat mode

               ------------------------
               -- Process_Mode_Break --
               ------------------------

               procedure Process_Mode_Break is
                  use VSS.Strings;

                  type Format_State_Array is
                    array (Positive range <>) of Format_State_Record;

                  Alignment_Table_Elements   : Document_Table
                    renames Document
                              .Bare_Document
                              .Command
                              .Alignment_Table_Elements;
                  Alignment_Table_Separators : Document_Table
                    renames Document
                              .Bare_Document
                              .Command
                              .Alignment_Table_Separators;

                  States :
                    Format_State_Array
                      (Alignment_Table_Elements.First_Index
                       .. Alignment_Table_Elements.Last_Index) :=
                      [others => Format_State];
                  --  The state for each row

                  First_Row_Index    : constant Positive :=
                    Alignment_Table_Elements.First_Index;
                  Last_Row_Index     : constant Positive :=
                    Alignment_Table_Elements.Last_Index;
                  First_Column_Index : constant Positive := 1;
                  Last_Column_Index  : constant Positive :=
                    [for Row_Elements of Alignment_Table_Elements
                     => Row_Elements.Last_Index]'Reduce (Positive'Max, 1);

                  Max_Line_Length : Natural := 0;

               begin
                  Prettier_Ada_Trace.Trace
                    ("On Process_Document_Command_Alignment_Table."
                     & "Process_Mode_Break");

                  for State of States loop
                     State.Result := Empty_Prettier_String;
                  end loop;

                  if Format_State.Last_Was_Hardline then
                     --  If the last document appended to Format_State.Result
                     --  was a hardline, then we need to move any indentation
                     --  from Format_State.Result into
                     --  States (First_Row_Index).Result. Otherwise, if the
                     --  first document of the first row is a line break,
                     --  previously added identation is not removed, resulting
                     --  in trailing whitespaces in the final
                     --  Format_State.Result.

                     Trim (Format_State.Result, Right);
                     States (First_Row_Index)
                       .Result
                       .Text
                       .Append (Indentation.Value.Text);
                  end if;

                  Prettier_Ada_Trace.Trace
                    ("On Column_Index " & First_Column_Index'Image);

                  for Row_Index in First_Row_Index .. Last_Row_Index loop
                     Prettier_Ada_Trace.Trace
                       ("On Row_Index " & Row_Index'Image);
                     declare
                        Separator_Display_Width :
                          constant Display_Cell_Count :=
                            (if not Alignment_Table_Separators
                                      .Reference (Row_Index)
                                      .Is_Empty
                             then
                               Alignment_Table_Separators
                                 .Reference (Row_Index)
                                 .Reference (First_Column_Index)
                                 .Bare_Document
                                 .Command
                                 .Alignment_Table_Separator_Text
                                 .Display_Width
                            else 0);
                        Next_Options            :
                          constant Format_Options_Type :=
                            Format_Options_Type'
                               (Width       =>
                                  (if Natural (Separator_Display_Width)
                                      > Format_Options.Width
                                   then 0
                                   else
                                     Format_Options.Width
                                     - Natural (Separator_Display_Width)),
                                Indentation => Format_Options.Indentation,
                                End_Of_Line => Format_Options.End_Of_Line);
                        --  The formatting options for each element need
                        --  to have an offset on the maximum width to account
                        --  the separator's width.

                     begin
                        States (Row_Index).Should_Remeasure := False;

                        --  Check if this is the last table element. If so,
                        --  include the remaining print documents preceded
                        --  by a Command_Alignment_Table_End. The Fits
                        --  function ignores this command, but the
                        --  Format function does not. This ensures that the
                        --  remaining Print_Documents are taken into account
                        --  when checking if the table element should break
                        --  or not.

                        if Row_Index = Last_Row_Index
                          and then First_Column_Index = Last_Column_Index
                        then
                           States (Row_Index).Print_Commands :=
                             Format_State.Print_Commands;
                           States (Row_Index).Print_Commands.Append
                             (Print_Command_Type'
                                (Indentation,
                                 Mode_Break,
                                 Wrap_Command
                                   (new Command_Type'
                                      (Kind =>
                                         Command_Alignment_Table_End))));
                           States (Row_Index).Print_Commands.Append
                             (Print_Command_Type'
                                (Indentation,
                                 Mode_Break,
                                 Alignment_Table_Elements
                                   .Reference (Row_Index)
                                   .Reference (First_Column_Index)));
                        else
                           States (Row_Index).Print_Commands :=
                             [Print_Command_Type'
                                (Indentation,
                                 Mode_Break,
                                 Alignment_Table_Elements
                                   .Reference (Row_Index)
                                   .Reference (First_Column_Index))];
                        end if;

                        Format (States (Row_Index), Next_Options);

                        Prettier_Ada_Trace.Trace
                          ("States.State.Current_Line_Length"
                           & States (Row_Index).Current_Line_Length'Image);

                        --  Compute the Max_Line_Length seen so far. If there's
                        --  not separators on this row, then this row line
                        --  length should not affect the alignment of others.

                        if not Alignment_Table_Separators (Row_Index).Is_Empty
                        then
                           Max_Line_Length :=
                             Natural'Max
                               (@, States (Row_Index).Current_Line_Length);
                        end if;
                     end;
                  end loop;

                  Prettier_Ada_Trace.Trace
                    ("Max_Line_Length" & Max_Line_Length'Image);

                  for Column_Index
                     in Standard."+" (First_Column_Index, 1)
                        .. Last_Column_Index
                  loop
                     Prettier_Ada_Trace.Trace
                       ("On Column_Index " & Column_Index'Image);

                     --  Add padding to the buffers

                     Prettier_Ada_Trace.Trace
                       ("Adding padding to the buffers");

                     for Row_Index in First_Row_Index .. Last_Row_Index loop
                        Prettier_Ada_Trace.Trace
                          ("On Row_Index " & Row_Index'Image);

                        if Column_Index
                           <= Alignment_Table_Elements
                                .Element (Row_Index)
                                .Last_Index
                        then
                           declare
                              Padding : constant Character_Count :=
                                Character_Count
                                  (Max_Line_Length
                                   - States (Row_Index).Current_Line_Length);

                           begin
                              Prettier_Ada_Trace.Trace
                                ("Padding " & Padding'Image);

                              if Padding /= 0 then
                                 States (Row_Index)
                                   .Result
                                   .Text
                                   .Append (Padding * ' ');
                                 States (Row_Index).Current_Line_Length :=
                                   Max_Line_Length;
                              end if;
                           end;
                        end if;

                     end loop;

                     Prettier_Ada_Trace.Trace
                       ("Adding separators to the buffers");

                     --  Add separators to the buffers

                     for Row_Index in First_Row_Index .. Last_Row_Index loop
                        Prettier_Ada_Trace.Trace
                          ("On Row_Index " & Row_Index'Image);

                        if Standard."-" (Column_Index, 1)
                           <= Alignment_Table_Separators
                                .Element (Row_Index)
                                .Last_Index
                        then
                           declare
                              Separator : constant Document_Type :=
                                Alignment_Table_Separators
                                  .Constant_Reference (Row_Index)
                                  .Constant_Reference
                                     (Standard."-" (Column_Index, 1));

                              Separator_Display_Width : constant Natural :=
                                Natural
                                  (Separator
                                     .Bare_Document
                                     .Command
                                     .Alignment_Table_Separator_Text
                                     .Display_Width);

                              Row_Line_Length_After_Separator :
                                constant Natural :=
                                  Max_Line_Length + Separator_Display_Width;

                           begin
                              Prettier_Ada_Trace.Trace
                                ("Row_Line_Length_After_Separator "
                                 & Row_Line_Length_After_Separator'Image);

                              States (Row_Index)
                                .Result
                                .Text
                                .Append
                                   (Separator
                                      .Bare_Document
                                      .Command
                                      .Alignment_Table_Separator_Text
                                      .Text);
                              States (Row_Index).Current_Line_Length :=
                                Row_Line_Length_After_Separator;
                           end;
                        end if;
                     end loop;

                     --  Format current element and add it to the buffers

                     Prettier_Ada_Trace.Trace
                       ("Format current element and add it to the buffers");

                     Max_Line_Length := 0;

                     for Row_Index in First_Row_Index .. Last_Row_Index loop
                        Prettier_Ada_Trace.Trace
                          ("On Row_Index " & Row_Index'Image);

                        if Column_Index
                           <= Alignment_Table_Elements
                                .Constant_Reference (Row_Index)
                                .Last_Index
                        then
                           declare
                              Separator_Display_Width :
                                constant Display_Cell_Count :=
                                  (if Alignment_Table_Separators
                                        .Constant_Reference (Row_Index)
                                        .Last_Index
                                      >= Column_Index
                                   then
                                     Alignment_Table_Separators
                                       .Constant_Reference (Row_Index)
                                       .Constant_Reference (Column_Index)
                                       .Bare_Document
                                       .Command
                                       .Alignment_Table_Separator_Text
                                       .Display_Width
                                  else 0);
                              Next_Options :
                                constant Format_Options_Type :=
                                   Format_Options_Type'
                                      (Width       =>
                                         (if Natural (Separator_Display_Width)
                                             > Format_Options.Width
                                          then
                                            0
                                          else
                                            Format_Options.Width
                                            - Natural
                                                (Separator_Display_Width)),
                                       Indentation =>
                                         Format_Options.Indentation,
                                       End_Of_Line =>
                                         Format_Options.End_Of_Line);

                           begin
                              States (Row_Index).Should_Remeasure := False;

                              --  Check if this is the last table element. If
                              --  so, include the remaining print documents
                              --  preceded by a Command_Alignment_Table_End.
                              --  The Fits function ignores this command, but
                              --  the Format function does not. This ensures
                              --  that the remaining Print_Documents are taken
                              --  into account when checking if the table
                              --  element should break or not.

                              if Row_Index = Last_Row_Index
                                and then Column_Index
                                         = Alignment_Table_Elements
                                             .Constant_Reference (Row_Index)
                                             .Last_Index
                              then
                                 States (Row_Index).Print_Commands :=
                                   Format_State.Print_Commands;
                                 States (Row_Index).Print_Commands.Append
                                   (Print_Command_Type'
                                      (Indentation,
                                       Mode_Break,
                                       Wrap_Command
                                         (new Command_Type'
                                            (Kind =>
                                               Command_Alignment_Table_End))));
                                 States (Row_Index).Print_Commands.Append
                                   (Print_Command_Type'
                                      (Indentation,
                                       Mode_Break,
                                       Alignment_Table_Elements
                                         .Reference (Row_Index)
                                         .Reference (Column_Index)));
                              else
                                 States (Row_Index).Print_Commands :=
                                   [Print_Command_Type'
                                      (Indentation,
                                       Mode_Break,
                                       Alignment_Table_Elements
                                         .Reference (Row_Index)
                                         .Reference (Column_Index))];
                              end if;

                              Format (States (Row_Index), Next_Options);

                              Prettier_Ada_Trace.Trace
                                ("States.State.Current_Line_Length"
                                 & States (Row_Index)
                                     .Current_Line_Length'Image);

                              if not (Alignment_Table_Separators (Row_Index)
                                        .Last_Index
                                      < Column_Index)
                              then
                                 Max_Line_Length :=
                                   Natural'Max
                                     (@,
                                      States (Row_Index).Current_Line_Length);
                              end if;
                           end;
                        end if;
                     end loop;
                  end loop;

                  --  Finally, concatenate all buffers and add them to Result

                  Format_State.Result
                    .Text
                    .Append (States (First_Row_Index).Result.Text);
                  for Row_Index
                     in Standard."+" (First_Row_Index, 1) .. Last_Row_Index
                  loop
                     Format_State.Result.Text.Append (End_Of_Line.Text);
                     if not States (Row_Index).Result.Text.Starts_With
                              (End_Of_Line.Text)
                     then
                        Format_State.Result.Text.Append
                          (Indentation.Value.Text);
                     end if;
                     Format_State.Result.Text.Append
                       (States (Row_Index).Result.Text);
                  end loop;

                  --  Also update the Format_State.Current_Line_Length based on
                  --  the last table row.

                  Format_State.Current_Line_Length :=
                    States (Last_Row_Index).Current_Line_Length;

               end Process_Mode_Break;

               -----------------------
               -- Process_Mode_Flat --
               -----------------------

               procedure Process_Mode_Flat is
                  Remaining_Line_Length : constant Integer :=
                    Format_Options.Width - Format_State.Current_Line_Length;
                  Has_Line_Suffix       : constant Boolean :=
                    Format_State.Line_Suffix.Length > 0;

               begin
                  Prettier_Ada_Trace.Trace
                    ("On Process_Document_Command_Alignment_Table."
                     & "Process_Mode_Flat");

                  Prettier_Ada_Trace.Trace ("Checking if table fits");
                  Prettier_Ada_Trace.Trace
                    ("Remaining_Line_Length: " & Remaining_Line_Length'Image);

                  if not Fits
                           (Print_Command,
                            [],
                            Remaining_Line_Length,
                            Has_Line_Suffix,
                            Format_State.Group_Mode_Map)
                  then
                     Prettier_Ada_Trace.Trace ("Table does not fit");
                     Process_Mode_Break;

                     return;
                  end if;

                  Prettier_Ada_Trace.Trace ("Table fits");

                  declare
                     Alignment_Table_Elements   : Document_Table
                       renames Document
                                 .Bare_Document
                                 .Command
                                 .Alignment_Table_Elements;
                     Alignment_Table_Separators : Document_Table
                       renames Document
                                 .Bare_Document
                                 .Command
                                 .Alignment_Table_Separators;

                     First_Row_Index    : constant Positive :=
                       Alignment_Table_Elements.First_Index;
                     Last_Row_Index     : constant Positive :=
                       Alignment_Table_Elements.Last_Index;
                     First_Column_Index : constant Positive := 1;
                     Last_Column_Index  : constant Positive :=
                       [for Row_Elements of Alignment_Table_Elements
                        => Row_Elements.Last_Index]'Reduce (Positive'Max, 1);

                  begin
                     --  Add the last row

                     for Column_Index
                       in reverse First_Column_Index .. Last_Column_Index
                     loop
                        if Alignment_Table_Separators
                             .Constant_Reference (Last_Row_Index)
                             .Last_Index
                           >= Column_Index
                        then
                           Format_State.Print_Commands.Append
                             (Print_Command_Type'
                                (Indentation,
                                 Mode_Flat,
                                 Alignment_Table_Separators
                                   .Constant_Reference (Last_Row_Index)
                                   .Constant_Reference (Column_Index)));
                        end if;

                        if Alignment_Table_Elements
                             .Constant_Reference (Last_Row_Index)
                             .Last_Index
                           >= Column_Index
                        then
                           Format_State.Print_Commands.Append
                             (Print_Command_Type'
                                (Indentation,
                                 Mode_Flat,
                                 Alignment_Table_Elements
                                   .Constant_Reference (Last_Row_Index)
                                   .Constant_Reference (Column_Index)));
                        end if;
                     end loop;

                     --  Add the remaining rows (in reverse order) with a Line
                     --  in between.

                     for Row_Index
                       in reverse First_Row_Index
                                  .. Standard."-" (Last_Row_Index, 1)
                     loop
                        Format_State.Print_Commands.Append
                          (Print_Command_Type'
                             (Indentation,
                              Mode_Flat,
                                Prettier_Ada
                                  .Documents
                                  .Builders
                                  .Line));

                        for Column_Index
                          in reverse First_Column_Index .. Last_Column_Index
                        loop
                           if Alignment_Table_Separators
                                .Constant_Reference (Row_Index)
                                .Last_Index
                              >= Column_Index
                           then
                              Format_State.Print_Commands.Append
                                (Print_Command_Type'
                                   (Indentation,
                                    Mode_Flat,
                                    Alignment_Table_Separators
                                      .Constant_Reference (Row_Index)
                                      .Constant_Reference (Column_Index)));
                           end if;

                           if Alignment_Table_Elements
                                .Constant_Reference (Row_Index)
                                .Last_Index
                              >= Column_Index
                           then
                              Format_State.Print_Commands.Append
                                (Print_Command_Type'
                                   (Indentation,
                                    Mode_Flat,
                                    Alignment_Table_Elements
                                      .Constant_Reference (Row_Index)
                                      .Constant_Reference (Column_Index)));
                           end if;
                        end loop;
                     end loop;
                  end;
               end Process_Mode_Flat;

            begin
               Prettier_Ada_Trace.Trace
                 ("On Process_Document_Command_Alignment_Table");

               if Document.Bare_Document.Command.Alignment_Table_Must_Break
                 or Mode in Mode_Break
               then
                  Prettier_Ada_Trace.Trace ("Table must break");
                  Process_Mode_Break;

               else
                  Process_Mode_Flat;
               end if;
            end Process_Document_Command_Alignment_Table;

            --------------------------------------------------
            -- Process_Document_Command_Alignment_Separator --
            --------------------------------------------------

            procedure Process_Document_Command_Alignment_Table_Separator
            is
            begin
               Append
                 (Format_State.Result,
                  Document
                    .Bare_Document
                    .Command
                    .Alignment_Table_Separator_Text);
               Format_State.Current_Line_Length :=
                 @
                 + Document
                     .Bare_Document
                     .Command
                     .Alignment_Table_Separator_Text
                     .Display_Width;
            end Process_Document_Command_Alignment_Table_Separator;

         begin
            Format_State.Print_Commands.Delete_Last;

            --  If No_Document, skip it

            if Document = No_Document then
               goto Continue;
            end if;

            case Document.Bare_Document.Kind is
               when Document_Text =>
                  Process_Document_Text;
                  Format_State.Last_Was_Hardline := False;

               when Document_List =>
                  Process_Document_List;

               when Document_Command =>
                  case Document.Bare_Document.Command.Kind is
                     when Command_Cursor =>
                        if Format_State.Printed_Cursor_Count >= 2 then
                           --  TODO: Replace this by a GNATfmt defined
                           --  exception
                           raise Program_Error;
                        end if;
                        --  TODO: How to add this command to Result?
                        --  Should Result be instead a Document_Type_Vector
                        --  with only Document_Text and Command_Cursor?
                        Format_State.Printed_Cursor_Count := @ + Natural (1);

                     when Command_Indent =>
                        Format_State.Print_Commands.Append
                          (Print_Command_Type'
                             (Make_Indentation
                                (Indentation, Format_Options.Indentation),
                              Mode,
                              Document.Bare_Document.Command.Indent_Contents));

                     when Command_Align =>
                        Format_State.Print_Commands.Append
                          (Print_Command_Type'
                             (Make_Align
                                (Indentation,
                                 Document.Bare_Document.Command.Align_Data,
                                 Format_Options,
                                 Format_State.Current_Line_Length),
                              Mode,
                              Document.Bare_Document.Command.Align_Contents));

                     when Command_Trim =>
                        Format_State.Current_Line_Length :=
                          @ - Trim (Format_State.Result);
                        Format_State.Last_Was_Hardline := False;

                     when Command_Group =>
                        Process_Document_Command_Group;

                     when Command_Fill =>
                        Process_Document_Command_Fill;

                     when Command_If_Break =>
                        declare
                           Group_Mode : constant Mode_Kind :=
                             (if Document
                                   .Bare_Document
                                   .Command
                                   .If_Break_Group_Id
                                 /= No_Symbol
                              then
                                 (if Format_State.Group_Mode_Map.Contains
                                      (Document
                                         .Bare_Document
                                         .Command
                                         .If_Break_Group_Id)
                                  then
                                    Format_State.Group_Mode_Map.Element
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
                                       Format_State.Print_Commands.Append
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
                                       Format_State.Print_Commands.Append
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
                        declare
                           Group_Mode : constant Mode_Kind :=
                             (if Document
                                   .Bare_Document
                                   .Command
                                   .Indent_If_Break_Group_Id
                                 /= No_Symbol
                                and then Format_State.Group_Mode_Map.Contains
                                           (Document
                                              .Bare_Document
                                              .Command
                                              .Indent_If_Break_Group_Id)
                              then
                                 Format_State.Group_Mode_Map.Element
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
                                    Format_State.Print_Commands.Append
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
                                    Format_State.Print_Commands.Append
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
                        Format_State.Line_Suffix.Append
                          (Print_Command_Type'
                             (Indentation,
                              Mode,
                              Document
                                .Bare_Document
                                .Command
                                .Line_Suffix_Contents));

                     when Command_Line_Suffix_Boundary =>
                        if not Format_State.Line_Suffix.Is_Empty then
                           Format_State.Print_Commands.Append
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
                        Format_State.Print_Commands.Append
                          (Print_Command_Type'
                             (Indentation,
                              Mode,
                              Document.Bare_Document.Command.Label_Contents));

                     when Command_Break_Parent =>
                        null;

                     when Command_Alignment_Table =>
                        Process_Document_Command_Alignment_Table;

                     when Command_Alignment_Table_Separator =>
                        Process_Document_Command_Alignment_Table_Separator;

                     when Command_Alignment_Table_End =>
                        exit;
                  end case;
            end case;

            <<Continue>>

            if Format_State.Print_Commands.Length = 0
               and Format_State.Line_Suffix.Length > 0
            then
               for Suffix of reverse Format_State.Line_Suffix loop
                  Format_State.Print_Commands.Append (Suffix);
               end loop;
               Format_State.Line_Suffix.Clear;
            end if;
         end;
      end loop;

      Prettier_Ada.Documents.Builders.Reset_Document_Id;

   exception
      when others =>
         Prettier_Ada.Documents.Builders.Reset_Document_Id;
         raise;
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

      Flushed_Tabs : Natural := 0;

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
            Flushed_Tabs := @ + Last_Tabs;
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
               if Part.Margin > Natural (Value.Display_Width) then
                  --  Last_Tabs and Last_Spaces are not flushed only when
                  --  Part.Kind = Number_Align. By simply adjusting Last_Spaces
                  --  based on the current line length, any previously
                  --  unflushed Last_Tabs and Last_Spaces become irrelevant,
                  --  and the number of tabs added when Part.Kind = Indent is
                  --  kept.

                  Last_Spaces := Part.Margin - Value.Display_Width;

               elsif Part.Margin < Natural (Value.Display_Width) then
                  --  This is only possible when there are String_Align before
                  --  Inner_Roots. Keep the flushed tabs and fill with spaces.
                  --
                  --  Note: If the String_Align's Texts were not whitespaces,
                  --  then it becomes unclear what the user wanted to do.
                  --  Inner root assumes that previous indentation was tabs
                  --  followed by spaces only.

                  Value.Text.Clear;
                  Value.Display_Width := 0;
                  Add_Tabs (Flushed_Tabs);
                  Last_Spaces := Part.Margin - Value.Display_Width;
               end if;

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
      use VSS.Strings;
      use VSS.Characters.Latin;

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
                    To_Prettier_String (Align_Data.T)),
                 Options.Indentation);

         when Continuation_Line_Indent =>
            return
              Generate_Indentation
                (From,
                 Indentation_Data_Type'
                   (String_Align,
                    (Character_Count (Options.Indentation.Continuation)
                     * Space,
                     Display_Cell_Count (Options.Indentation.Continuation))),
                 Options.Indentation);

         when Dedent_To_Root =>
            if From.Root = null then
               return Root_Indent (Options.Indentation);
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
      Table_Stack     : Document_Vector;

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
         if Document = No_Document then
            return Optional_Boolean'(Is_Set => False);
         end if;

         case Document.Bare_Document.Kind is
            when Document_Command =>
               case Document.Bare_Document.Command.Kind is
                  when Command_Break_Parent =>
                     Break_Parent_Group (Group_Stack);
                     Break_Parent_Table (Table_Stack);

                  when Command_Group =>
                     Group_Stack.Append (Document);
                     if Already_Visited.Contains (Document) then
                        return
                          Optional_Boolean'
                            (Is_Set => True, Value => False);
                     end if;
                     Already_Visited.Insert (Document);

                  when Command_Alignment_Table =>
                     Table_Stack.Append (Document);
                     if Already_Visited.Contains (Document) then
                        return
                          Optional_Boolean'
                            (Is_Set => True, Value => False);
                     end if;
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
         if Document = No_Document then
            return Optional_Boolean'(Is_Set => False);
         end if;

         case Document.Bare_Document.Kind is
            when Document_Command =>
               if Document.Bare_Document.Command.Kind in Command_Group then
                  declare
                     Group : constant Document_Type :=
                       Group_Stack.Last_Element;

                  begin
                     Group_Stack.Delete_Last;
                     if Group.Bare_Document.Command.Break then
                        Break_Parent_Group (Group_Stack);
                        Break_Parent_Table (Table_Stack);
                     end if;
                  end;

               elsif Document.Bare_Document.Command.Kind
                 in Command_Alignment_Table
               then
                  declare
                     Table : constant Document_Type :=
                       Table_Stack.Last_Element;

                  begin
                     Table_Stack.Delete_Last;
                     if Table.Bare_Document.Command.Break_Parents then
                        Break_Parent_Group (Group_Stack);
                        Break_Parent_Table (Table_Stack);
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

   function Root_Indent
     (Options : Indentation_Options_Type)
      return Indentation_Queue_Type
   is
   begin
      if Options.Offset = (Tabs => 0, Spaces => 0) then
         return
           Indentation_Queue_Type'
             (Value  => Empty_Prettier_String,
              Queue  => [],
              Root   => null);

      else
         declare
            Queue : Indentation_Data_Vector;

         begin
            Queue.Append
              ((Kind => Indent),
               Ada.Containers.Count_Type (Options.Offset.Tabs));
            Queue.Append
              (Indentation_Data_Type'
                 (Kind => String_Align,
                  Text =>
                    (VSS.Strings."*"
                       (VSS.Strings.Character_Count (Options.Offset.Spaces),
                        VSS.Characters.Latin.Space),
                     VSS.Strings.Display_Cell_Count (Options.Offset.Spaces))));

            return
              Indentation_Queue_Type'
                (Value  =>
                  (VSS.Strings."&"
                     (VSS.Strings."*"
                        (VSS.Strings.Character_Count (Options.Offset.Tabs),
                         VSS.Characters.Latin.Character_Tabulation),
                      VSS.Strings."*"
                        (VSS.Strings.Character_Count (Options.Offset.Spaces),
                         VSS.Characters.Latin.Space)),
                   VSS.Strings.Display_Cell_Count
                     (Options.Offset.Spaces
                      + Options.Offset.Tabs * Options.Width)),
                 Queue  => Queue,
                 Root   => null);
         end;
      end if;
   end Root_Indent;

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
          (Kind      => Document_Text,
           Ref_Count => 1,
           Text      =>
             (declare
                Text_VSS : constant VSS.Strings.Virtual_String :=
                  VSS.Strings.To_Virtual_String (Text);
              begin
                (Text_VSS, VSS.Strings.Utilities.Display_Width (Text_VSS))),
           Id        => New_Document_Id);

   begin
      return (Ada.Finalization.Controlled with Bare_Document => Bare_Document);
   end To_Document_Type;

   ------------------------
   -- To_Prettier_String --
   ------------------------

   function To_Prettier_String
     (Text : Ada.Strings.Unbounded.Unbounded_String) return Prettier_String
   is
      Text_VSS : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String (Text);
   begin
      return (Text_VSS, VSS.Strings.Utilities.Display_Width (Text_VSS));
   end To_Prettier_String;

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
         declare
            Doc : constant Document_Type := Doc_Stack.Last_Element;

         begin
            Doc_Stack.Delete_Last;

            --  If No_Document, skip it

            if Doc = No_Document then
               goto Continue;
            end if;

            if Doc = Traverse_Doc_On_Exit_Stack_Marker then
               declare
                  Doc : constant Document_Type := Doc_Stack.Last_Element;
                  Ignore : constant Optional_Boolean := On_Exit (Doc);

               begin
                  Doc_Stack.Delete_Last;

                  goto Continue;
               end;
            end if;

            if On_Exit /= null then
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
                     goto Continue;
                  end if;
               end;
            end if;

            case Doc.Bare_Document.Kind is
               when Document_Text =>
                  null;

               when Document_List =>
                  declare
                     Documents : constant Document_Vector :=
                       Doc.Bare_Document.List;

                  begin
                     for Child_Doc of reverse Documents loop
                        Doc_Stack.Append (Child_Doc);
                     end loop;
                  end;

               when Document_Command =>
                  case Doc.Bare_Document.Command.Kind is
                     when Command_Align =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Align_Contents);

                     when Command_Indent =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Indent_Contents);

                     when Command_Indent_If_Break =>
                        Doc_Stack.Append
                          (Doc
                             .Bare_Document
                             .Command
                             .Indent_If_Break_Contents);

                     when Command_Label =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Label_Contents);

                     when Command_Line_Suffix =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Line_Suffix_Contents);

                     when Command_Fill =>
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
                              Doc_Stack.Append (Part);
                           end loop;
                        end;

                     when Command_Group =>
                        if Should_Traverse_Conditional_Groups
                          and Doc
                                .Bare_Document
                                .Command
                                .Expanded_States
                              /= No_Document
                        then
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
                                 Doc_Stack.Append (State);
                              end loop;
                           end;

                        else
                           Doc_Stack.Append
                             (Doc.Bare_Document.Command.Group_Contents);
                        end if;

                     when Command_If_Break =>
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Flat_Contents);
                        Doc_Stack.Append
                          (Doc.Bare_Document.Command.Break_Contents);

                     when Command_Break_Parent
                          | Command_Cursor
                          | Command_Line
                          | Command_Line_Suffix_Boundary
                          | Command_Trim
                          | Command_Alignment_Table_End
                     =>
                        null;

                     when Command_Alignment_Table =>
                        for Row_Index
                          in Doc
                               .Bare_Document
                               .Command
                               .Alignment_Table_Elements
                               .First_Index
                             .. Doc
                                  .Bare_Document
                                  .Command
                                  .Alignment_Table_Elements
                                  .Last_Index
                        loop
                           declare
                              First_Column_Index : constant Positive :=
                                Doc
                                  .Bare_Document
                                  .Command
                                  .Alignment_Table_Elements
                                  .Constant_Reference (Row_Index)
                                  .First_Index;
                              Last_Column_Index : constant Positive :=
                                Doc
                                  .Bare_Document
                                  .Command
                                  .Alignment_Table_Elements
                                  .Constant_Reference (Row_Index)
                                  .Last_Index;

                           begin
                              Doc_Stack.Append
                                (Doc
                                   .Bare_Document
                                   .Command
                                   .Alignment_Table_Elements
                                   .Constant_Reference
                                      (Row_Index)
                                   .Constant_Reference
                                      (Last_Column_Index));

                              for Column_Index
                                in reverse First_Column_Index
                                           .. Standard."-"
                                                (Last_Column_Index, 1)
                              loop
                                 Doc_Stack.Append
                                   (Doc
                                      .Bare_Document
                                      .Command
                                      .Alignment_Table_Separators
                                      .Constant_Reference
                                         (Row_Index)
                                      .Constant_Reference
                                         (Column_Index));
                                 Doc_Stack.Append
                                   (Doc
                                      .Bare_Document
                                      .Command
                                      .Alignment_Table_Elements
                                      .Constant_Reference
                                         (Row_Index)
                                      .Constant_Reference
                                         (Column_Index));
                              end loop;
                           end;
                        end loop;

                     when Command_Alignment_Table_Separator =>
                        null;
                  end case;
            end case;

            <<Continue>>
         end;
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
      Item.Display_Width :=
        (if Trim_Count > Item.Display_Width
         then 0
         else Item.Display_Width - Trim_Count);
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
