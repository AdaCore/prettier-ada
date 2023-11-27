--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings.Conversions;

package body Prettier_Ada.Documents.Builders is

   Document_Id : Natural := Natural'First;

   ---------
   -- "+" --
   ---------

   function "+" (Documents : Document_Vector) return Document_Array is
     ([for Document of Documents => Document]);
   pragma Inline ("+");

   ---------------------
   -- New_Document_Id --
   ---------------------

   function New_Document_Id return Natural is
   begin
      return Result : constant Natural := Document_Id do
         Document_Id := @ + 1;
      end return;
   end New_Document_Id;

   -----------------------
   -- Reset_Document_Id --
   -----------------------

   procedure Reset_Document_Id is
   begin
      Document_Id := Natural'First;
   end Reset_Document_Id;

   ----------
   -- Text --
   ----------

   function Text
     (T : Ada.Strings.Unbounded.Unbounded_String)
      return Document_Type
   is
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind => Document_Text,
           Text => VSS.Strings.Conversions.To_Virtual_String (T),
           Id   => New_Document_Id);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Text;

   -----------
   --  List --
   -----------

   function List
     (Documents : Document_Array)
      return Document_Type
   is
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind => Document_List,
           Id   => New_Document_Id,
           List => new Document_Array'(Documents));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end List;

   -----------
   -- Align --
   -----------

   function Align
     (Data     : Alignment_Data_Type;
      Contents : Document_Type)
      return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind            => Command_Align,
         Align_Data      => Data,
         Align_Contents  => Contents);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Align;

   -----------
   -- Align --
   -----------

   function Align
     (Data     : Alignment_Data_Type;
      Contents : Document_Array)
      return Document_Type
   is (Align (Data, List (Contents)));

   ------------------
   -- Break_Parent --
   ------------------

   function Break_Parent return Document_Type
   is
      Command       : constant Command_Type := (Kind => Command_Break_Parent);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Break_Parent;

   ------------
   -- Cursor --
   ------------

   function Cursor return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind => Command_Cursor, Place_Holder => New_Symbol);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Cursor;

   ----------
   -- Fill --
   ----------

   function Fill
     (Parts : Document_Type)
      return Document_Type
   is
      Command        : constant Command_Type :=
        (Kind   => Command_Fill,
         Parts  => Parts);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Fill;

   ----------
   -- Fill --
   ----------

   function Fill
     (Parts : Document_Array)
      return Document_Type
   is (Fill (List (Parts)));

   -----------
   -- Group --
   -----------

   function Group
     (Documents : Document_Type;
      Options   : Group_Options_Type := No_Group_Options)
      return Document_Type
   is
      Command        : constant Command_Type :=
        (Kind            => Command_Group,
         Id              => Options.Id,
         Group_Contents  => Documents,
         Break           => Options.Should_Break,
         Expanded_States => Options.Expanded_States);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Group;

   -----------
   -- Group --
   -----------

   function Group
     (Documents : Document_Array;
      Options   : Group_Options_Type := No_Group_Options)
      return Document_Type
   is (Group (List (Documents), Options));

   --------------
   -- If_Break --
   --------------

   function If_Break
     (Break_Contents : Document_Type;
      Flat_Contents  : Document_Type := No_Document;
      Options        : If_Break_Options_Type := No_If_Break_Options)
      return Document_Type
   is
      Command        : constant Command_Type :=
        (Kind              => Command_If_Break,
         If_Break_Group_Id => Options.Group_Id,
         Break_Contents    => Break_Contents,
         Flat_Contents     => Flat_Contents);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end If_Break;

   ------------
   -- Indent --
   ------------

   function Indent (Contents : Document_Type) return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind            => Command_Indent,
         Indent_Contents => Contents);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Indent;

   ---------------------
   -- Indent_If_Break --
   ---------------------

   function Indent_If_Break
     (Contents : Document_Type;
      Options : Indent_If_Break_Options_Type := No_Indent_If_Break_Options)
      return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind                     => Command_Indent_If_Break,
         Indent_If_Break_Contents => Contents,
         Indent_If_Break_Group_Id => Options.Group_Id,
         Negate                   => Options.Negate);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Indent_If_Break;

   -----------
   -- Label --
   -----------

   function Label
     (Text     : Ada.Strings.Unbounded.Unbounded_String;
      Contents : Document_Type)
      return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind           => Command_Label,
         Text           => VSS.Strings.Conversions.To_Virtual_String (Text),
         Label_Contents => Contents);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => new Command_Type'(Command));

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Label;

   ----------
   -- Line --
   ----------

   function Line return Document_Type
   is
      Command       : constant Command_Access :=
        new Command_Type'
              (Kind    => Command_Line,
               Literal => False,
               Hard    => False,
               Soft    => False);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
         (Kind    => Document_Command,
          Id      => New_Document_Id,
          Command => Command);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Line;

   ---------------
   -- Soft_Line --
   ---------------

   function Soft_Line return Document_Type
   is
      Command       : constant Command_Access :=
        new Command_Type'
              (Kind    => Command_Line,
               Literal => False,
               Hard    => False,
               Soft    => True);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => Command);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Soft_Line;

   ---------------
   -- Hard_Line --
   ---------------

   function Hard_Line return Document_Type
   is
      Command       : constant Command_Access :=
        new Command_Type'
              (Kind    => Command_Line,
               Literal => False,
               Hard    => True,
               Soft    => False);
      Hard_Line_Bare_Document      : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => Command);
      Hard_Line_Document : constant Document_Type :=
        (Bare_Document => Hard_Line_Bare_Document);

   begin
      return List (Document_Array'([Hard_Line_Document, Break_Parent]));
   end Hard_Line;

   ------------------
   -- Literal_Line --
   ------------------

   function Literal_Line return Document_Type
   is
      Hard_Line_Command       : constant Command_Access :=
        new Command_Type'
              (Kind    => Command_Line,
               Literal => True,
               Hard    => False,
               Soft    => False);
      Hard_Line_Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => Hard_Line_Command);
      Hard_Line_Document      : constant Document_Type :=
        (Bare_Document => Hard_Line_Bare_Document);

   begin
      return List (Document_Array'([Hard_Line_Document, Break_Parent]));
   end Literal_Line;

   ------------------------------------
   -- Hard_Line_Without_Break_Parent --
   ------------------------------------

   function Hard_Line_Without_Break_Parent return Document_Type
   is
      Command       : constant Command_Access :=
        new Command_Type'
              (Kind    => Command_Line,
               Literal => False,
               Hard    => True,
               Soft    => False);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => Command);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Hard_Line_Without_Break_Parent;

   ---------------------------------------
   -- Literal_Line_Without_Break_Parent --
   ---------------------------------------

   function Literal_Line_Without_Break_Parent return Document_Type
   is
      Command       : constant Command_Access :=
        new Command_Type'
              (Kind    => Command_Line,
               Literal => True,
               Hard    => False,
               Soft    => False);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind    => Document_Command,
           Id      => New_Document_Id,
           Command => Command);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Literal_Line_Without_Break_Parent;

   ---------------------
   -- New_Line_Suffix --
   ---------------------

   function Line_Suffix (Contents : Document_Type) return Document_Type
   is
      Command       : constant Command_Access :=
        new Command_Type'
          (Kind                 => Command_Line_Suffix,
           Line_Suffix_Contents => Contents);
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
         (Kind    => Document_Command,
          Id      => New_Document_Id,
          Command => Command);

   begin
      return Document_Type'(Bare_Document => Bare_Document);
   end Line_Suffix;

   ------------------------------
   -- New_Line_Suffix_Boundary --
   ------------------------------

   function Line_Suffix_Boundary return Document_Type
   is
      Command       : constant Command_Type :=
        (Kind => Command_Line_Suffix_Boundary);
      Bare_Document : constant Bare_Document_Record :=
        (Kind    => Document_Command,
         Id      => New_Document_Id,
         Command => new Command_Type'(Command));

   begin
      return
        Document_Type'
          (Bare_Document =>
              new Bare_Document_Record'(Bare_Document));
   end Line_Suffix_Boundary;

   ----------
   -- Trim --
   ----------

   function Trim return Document_Type
   is
      Command        : constant Command_Type      := (Kind => Command_Trim);
      Bare_Document : constant Bare_Document_Record :=
        (Kind    => Document_Command,
         Id      => New_Document_Id,
         Command => new Command_Type'(Command));

   begin
      return
        Document_Type'
          (Bare_Document =>
             new Bare_Document_Record'(Bare_Document));
   end Trim;

   ----------
   -- Join --
   ----------

   function Join
     (Separator : Document_Type;
      Documents : Document_Array)
      return Document_Type
   is
   begin
      if Documents'Length = 0 then
         return List (Document_Array'([]));

      elsif Documents'Length = 1 then
         declare
            Length : constant Positive := 2;
            Joined_Documents : Document_Array (1 .. Length);
         begin
            Joined_Documents (1) := Documents (Documents'First);
            Joined_Documents (2) := Separator;
            return List (Joined_Documents);
         end;

      else
         declare
            --  Double the size minus 1
            Last_Index       : constant Positive :=
              Documents'Last + Documents'Length - 1;
            Joined_Documents : Document_Array (Documents'First .. Last_Index);

         begin
            --  Joined_Documents'First + 2 * (J - Documents'First) means
            --  increment indices by 2 instead of 1.

            --  Copy from Documents
            for J in Documents'Range loop
               Joined_Documents
                 (Joined_Documents'First + 2 * (J - Documents'First)) :=
                   Documents (J);
            end loop;

            --  Fill the gaps with Separators.
            --  Gaps start on the second element, hence the + 1.
            --  There are Documents'Length - 1 separators, hence the
            --  Documents'Last - 1 as upper bound.
            for J in Documents'First .. Documents'Last - 1 loop
               Joined_Documents
                 (Joined_Documents'First + 2 * (J - Documents'First) + 1) :=
                   Separator;
            end loop;

            return List (Joined_Documents);
         end;
      end if;
   end Join;

end Prettier_Ada.Documents.Builders;
