--
--  Copyright (C) 2023-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Strings.Conversions;

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
           Text      => VSS.Strings.Conversions.To_Virtual_String (T),
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
     (Documents : Document_Type;
      Options   : Group_Options_Type := No_Group_Options)
      return Document_Type
   is
   begin
      return
        Wrap_Command
          (new Command_Type'
             (Kind            => Command_Group,
              Id              => Options.Id,
              Group_Contents  => Documents,
              Break           => Options.Should_Break,
              Expanded_States => Options.Expanded_States));
   end Group;

   -----------
   -- Group --
   -----------

   function Group
     (Documents : Document_Vector;
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
              Text           => VSS.Strings.Conversions.To_Virtual_String
                                  (Text),
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

end Prettier_Ada.Documents.Builders;
