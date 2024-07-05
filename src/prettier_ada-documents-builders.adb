--
--  Copyright (C) 2023-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Prettier_Ada.Documents.Implementation;
use Prettier_Ada.Documents.Implementation;

package body Prettier_Ada.Documents.Builders is

   function Wrap_Command
     (Command : Command_Access) return Document_Type
   is (Ada.Finalization.Controlled with
       Bare_Document => new Bare_Document_Record'
                              (Document_Command, 1, New_Document_Id, Command));
   --  Allocate a new document to wrap the given command

   ----------
   -- Text --
   ----------

   function Text
     (T : Ada.Strings.Unbounded.Unbounded_String)
      return Document_Type
   is
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind      => Document_Text,
           Ref_Count => 1,
           Text      => To_Prettier_String (T),
           Id        => New_Document_Id);

   begin
      return (Ada.Finalization.Controlled with Bare_Document => Bare_Document);
   end Text;

   -----------
   --  List --
   -----------

   function List
     (Documents : Document_Vector)
      return Document_Type
   is
      Bare_Document : constant Bare_Document_Access :=
        new Bare_Document_Record'
          (Kind      => Document_List,
           Ref_Count => 1,
           Id        => New_Document_Id,
           List      => Documents);

   begin
      return (Ada.Finalization.Controlled with Bare_Document => Bare_Document);
   end List;

   -----------
   -- Align --
   -----------

   function Align
     (Data     : Alignment_Data_Type;
      Contents : Document_Type)
      return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'(Kind            => Command_Align,
                             Align_Data      => Data,
                             Align_Contents  => Contents));
   end Align;

   -----------
   -- Align --
   -----------

   function Align
     (Data     : Alignment_Data_Type;
      Contents : Document_Vector)
      return Document_Type
   is (Align (Data, List (Contents)));

   ------------------
   -- Break_Parent --
   ------------------

   function Break_Parent return Document_Type
   is
   begin
      return Wrap_Command (new Command_Type'(Kind => Command_Break_Parent));
   end Break_Parent;

   ------------
   -- Cursor --
   ------------

   function Cursor return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'(Kind => Command_Cursor,
                             Place_Holder => New_Symbol));
   end Cursor;

   ----------
   -- Fill --
   ----------

   function Fill
     (Parts : Document_Type)
      return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind   => Command_Fill,
              Parts  =>
                (if Parts.Bare_Document.Kind = Document_List
                 then Parts
                 else List ([Parts]))));
   end Fill;

   ----------
   -- Fill --
   ----------

   function Fill
     (Parts : Document_Vector)
      return Document_Type
   is (Fill (List (Parts)));

   -----------
   -- Group --
   -----------

   function Group
     (Documents    : Document_Type;
      Id           : Symbol_Type := No_Symbol;
      Should_Break : Boolean     := False)
      return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind            => Command_Group,
              Id              => Id,
              Group_Contents  => Documents,
              Break           => Should_Break,
              Expanded_States => No_Document));
   end Group;

   -----------
   -- Group --
   -----------

   function Group
     (Documents    : Document_Vector;
      Id           : Symbol_Type := No_Symbol;
      Should_Break : Boolean     := False)
      return Document_Type
   is (Group (List (Documents), Id, Should_Break));

   -----------------------
   -- Conditional_Group --
   -----------------------

   function Conditional_Group
     (Alternatives : Document_Vector;
      Id           : Symbol_Type := No_Symbol;
      Should_Break : Boolean     := False)
      return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind            => Command_Group,
              Id              => Id,
              Group_Contents  =>
                (if Alternatives.Is_Empty
                 then No_Document
                 else Alternatives.First_Element),
              Break           => Should_Break,
              Expanded_States => List (Alternatives)));
   end Conditional_Group;
   --  Creates a new Conditional_Group Document Command

   --------------
   -- If_Break --
   --------------

   function If_Break
     (Break_Contents : Document_Type;
      Flat_Contents  : Document_Type := No_Document;
      Options        : If_Break_Options_Type := No_If_Break_Options)
      return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind              => Command_If_Break,
              If_Break_Group_Id => Options.Group_Id,
              Break_Contents    => Break_Contents,
              Flat_Contents     => Flat_Contents));
   end If_Break;

   ------------
   -- Indent --
   ------------

   function Indent (Contents : Document_Type) return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind            => Command_Indent,
              Indent_Contents => Contents));
   end Indent;

   ---------------------
   -- Indent_If_Break --
   ---------------------

   function Indent_If_Break
     (Contents : Document_Type;
      Options : Indent_If_Break_Options_Type := No_Indent_If_Break_Options)
      return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind                     => Command_Indent_If_Break,
              Indent_If_Break_Contents => Contents,
              Indent_If_Break_Group_Id => Options.Group_Id,
              Negate                   => Options.Negate));
   end Indent_If_Break;

   -----------
   -- Label --
   -----------

   function Label
     (Text     : Ada.Strings.Unbounded.Unbounded_String;
      Contents : Document_Type)
      return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind           => Command_Label,
              Text           => To_Prettier_String (Text),
              Label_Contents => Contents));
   end Label;

   ----------
   -- Line --
   ----------

   function Line return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind    => Command_Line,
              Literal => False,
              Hard    => False,
              Soft    => False));
   end Line;

   ---------------
   -- Soft_Line --
   ---------------

   function Soft_Line return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind    => Command_Line,
              Literal => False,
              Hard    => False,
              Soft    => True));
   end Soft_Line;

   ---------------
   -- Hard_Line --
   ---------------

   function Hard_Line return Document_Type
   is
      Literal_Line_Document : constant Document_Type :=
        Wrap_Command
          (new Command_Type'
             (Kind    => Command_Line,
              Literal => False,
              Hard    => True,
              Soft    => False));

   begin
      return List ([Literal_Line_Document, Break_Parent]);
   end Hard_Line;

   ------------------
   -- Literal_Line --
   ------------------

   function Literal_Line return Document_Type
   is
      Literal_Line_Document : constant Document_Type :=
        Wrap_Command
          (new Command_Type'
             (Kind    => Command_Line,
              Literal => True,
              Hard    => False,
              Soft    => False));

   begin
      return List ([Literal_Line_Document, Break_Parent]);
   end Literal_Line;

   ------------------------------------
   -- Hard_Line_Without_Break_Parent --
   ------------------------------------

   function Hard_Line_Without_Break_Parent return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind    => Command_Line,
              Literal => False,
              Hard    => True,
              Soft    => False));
   end Hard_Line_Without_Break_Parent;

   ---------------------------------------
   -- Literal_Line_Without_Break_Parent --
   ---------------------------------------

   function Literal_Line_Without_Break_Parent return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind    => Command_Line,
              Literal => True,
              Hard    => False,
              Soft    => False));
   end Literal_Line_Without_Break_Parent;

   ---------------------
   -- New_Line_Suffix --
   ---------------------

   function Line_Suffix (Contents : Document_Type) return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind                 => Command_Line_Suffix,
              Line_Suffix_Contents => Contents));
   end Line_Suffix;

   ------------------------------
   -- New_Line_Suffix_Boundary --
   ------------------------------

   function Line_Suffix_Boundary return Document_Type
   is
   begin
      return
        Wrap_Command (new Command_Type'(Kind => Command_Line_Suffix_Boundary));
   end Line_Suffix_Boundary;

   ----------
   -- Trim --
   ----------

   function Trim return Document_Type
   is
   begin
      return Wrap_Command (new Command_Type'(Kind => Command_Trim));
   end Trim;

   ----------
   -- Join --
   ----------

   function Join
     (Separator : Document_Type;
      Documents : Document_Vector)
      return Document_Type
   is
      use type Ada.Containers.Count_Type;

   begin
      if Documents.Length = 0 then
         return List ([]);

      elsif Documents.Length = 1 then
         return List ([Documents.First_Element, Separator]);

      else
         declare
            Joined_Documents : Document_Vector;

         begin
            for Document of Documents loop
               Joined_Documents.Append (Document);
               Joined_Documents.Append (Separator);
            end loop;
            Joined_Documents.Delete_Last;

            return List (Joined_Documents);
         end;
      end if;
   end Join;

   -----------------------
   -- Reset_Document_Id --
   -----------------------

   procedure Reset_Document_Id
     renames Prettier_Ada.Documents.Implementation.Reset_Document_Id;

   ---------------------
   -- Alignment_Table --
   ---------------------

   function Alignment_Table
     (Rows : Document_Table; Must_Break : Boolean := True) return Document_Type
   is
      Elements   : Prettier_Ada.Document_Vector_Vectors.Vector;
      Separators : Prettier_Ada.Document_Vector_Vectors.Vector;

      procedure Normalize_Table;
      --  Splits Rows into Elements and Separators.
      --  Separators are placed between elements so this ensures that for each
      --  row:
      --    Elements (row).Length = Separators (row).Length - 1
      --
      --  Example:
      --
      --  Rows: [[E11, S11, E12, S12, E13], [E21, S21, E22, S22, E23]]
      --
      --  Results in:
      --
      --  Elements: [[E11, E12, E13], [E21, E22, E23]]
      --  Separators: [[S11, S12], [S21, S22]]

      ---------------------
      -- Normalize_Table --
      ---------------------

      procedure Normalize_Table is
      begin
         --  Iterate through all rows

         for Row_Index in Rows.First_Index .. Rows.Last_Index loop
            declare
               use Prettier_Ada.Document_Vector_Vectors;

               use type Ada.Containers.Count_Type;

               Row : constant Constant_Reference_Type :=
                 Rows.Constant_Reference (Row_Index);
               --  This is the current row

               --  For each row, we will have a list of elements and a list
               --  of separators.

               Row_Elements   : Prettier_Ada.Document_Vectors.Vector;
               Row_Separators : Prettier_Ada.Document_Vectors.Vector;

               Elements_Aggregate : Prettier_Ada.Document_Vectors.Vector;
               --  Elements that follow another element are aggregated
               --  together.

            begin
               --  Iterate through all columns

               for Column_Index in Row.First_Index .. Row.Last_Index loop
                  declare
                     use Prettier_Ada.Document_Vectors;

                     Table_Element :
                       constant Prettier_Ada
                                  .Document_Vectors
                                  .Constant_Reference_Type :=
                         Row.Constant_Reference (Column_Index);

                     Is_Separator : constant Boolean :=
                       Table_Element.Bare_Document.Kind in Document_Command
                       and then Table_Element.Bare_Document.Command.Kind
                                in Command_Alignment_Table_Separator;
                     --  A table element can either be an element or a
                     --  separator.
                  begin
                     if Is_Separator then
                        --  Start by adding Elements_Aggregate to Row_Elements

                        if Elements_Aggregate.Is_Empty then
                           --  No elements before the separator. Simply add an
                           --  empty string.

                           Row_Elements.Append
                             (Text
                                (Ada.Strings.Unbounded.Null_Unbounded_String));

                        elsif Elements_Aggregate.Length = 1 then
                           Row_Elements.Append
                             (Elements_Aggregate.First_Element);

                        else
                           Row_Elements.Append (Group (Elements_Aggregate));
                        end if;

                        Elements_Aggregate.Clear;

                        --  Then add the separator to Row_Separators

                        Row_Separators.Append (Table_Element);

                     else
                        Elements_Aggregate.Append (Table_Element);
                     end if;
                  end;
               end loop;

               --  If the last element of this row is not a separator, then
               --  Elements_Aggregate won't be empty. If so, add it to
               --  Row_Elements.

                  if not Elements_Aggregate.Is_Empty then
                     if Elements_Aggregate.Length = 1 then
                        Row_Elements.Append (Elements_Aggregate.First_Element);

                     else
                        Row_Elements.Append (Group (Elements_Aggregate));
                     end if;
                  end if;

               --  Flush this row's elements and separators
               Elements.Append (Row_Elements);
               Separators.Append (Row_Separators);
            end;
         end loop;
      end Normalize_Table;

   begin
      Normalize_Table;

      return
        Wrap_Command
          (new Command_Type'
                 (Kind       => Command_Alignment_Table,
                  Alignment_Table_Elements   => Elements,
                  Alignment_Table_Separators => Separators,
                  Alignment_Table_Must_Break => Must_Break));
   end Alignment_Table;

   -------------------------------
   -- Alignment_Table_Separator --
   -------------------------------

   function Alignment_Table_Separator
     (Aligner_Text : Ada.Strings.Unbounded.Unbounded_String)
      return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
                 (Kind                           =>
                    Command_Alignment_Table_Separator,
                  Alignment_Table_Separator_Text =>
                    To_Prettier_String (Aligner_Text)));
   end Alignment_Table_Separator;

end Prettier_Ada.Documents.Builders;
