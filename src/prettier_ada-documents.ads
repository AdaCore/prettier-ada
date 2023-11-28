--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded;

private with Ada.Containers;
private with VSS.Strings;

--  This package provides the Document_Type definition (equivalent to
--  Prettier's Doc type) and format function.
--  To create a Document_Type, use the Prettier_Ada.Documents.Builders package.
package Prettier_Ada.Documents is

   type Indentation_Kind is (Spaces, Tabs);

   type Indentation_Options_Type is record
      Kind  : Indentation_Kind := Spaces;
      Width : Natural := 2;
   end record;

   type End_Of_Line_Kind is (LF, CR, CRLF);

   type Format_Options_Type is record
      Width       : Natural;
      Indentation : Indentation_Options_Type;
      End_Of_Line : End_Of_Line_Kind;
   end record;

   Default_Format_Options : constant Format_Options_Type :=
     (Width => 80, Indentation => (Spaces, 2), End_Of_Line => LF);

   type Document_Type is private
     with String_Literal => To_Document_Type;
   pragma Preelaborable_Initialization (Document_Type);
   --  TODO: This DS leaks memory. Design a way to release it.

   function To_Document_Type (Text : Wide_Wide_String) return Document_Type;
   --  Convert given string into virtual string.

   function Format
     (Document : Document_Type;
      Options  : Format_Options_Type := Default_Format_Options)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Formats Document with the given Options

   type Document_Array is array (Positive range <>) of Document_Type;

   type Symbol_Type is private;

   function New_Symbol return Symbol_Type;
   --  Returns a new Symbol_Type, used to uniquely identify parent
   --  Command_Group documents.

   --  TODO: Align_Data_Type is necessary for the the Document_Type and its
   --  builders but it does not fit well on the public part of this package.
   --  Try to make this private.

   type Align_Kind_Type is (Width, Text, Dedent, Dedent_To_Root, Root, None);

   type Alignment_Data_Type (Kind : Align_Kind_Type := None) is record
      case Kind is
         when Width =>
            N : Natural; -- Number of spaces or tabs
         when Text =>
            T : Ada.Strings.Unbounded.Unbounded_String;
         when Dedent | Dedent_To_Root | Root | None =>
            null;
      end case;
   end record;

private

   subtype Prettier_String is VSS.Strings.Virtual_String;

   function Hash (Symbol : Symbol_Type) return Ada.Containers.Hash_Type;
   --  Casts Symbol_Type as Hash_Type

   function Hash (Document : Document_Type) return Ada.Containers.Hash_Type;
   --  Casts the Document id as Hash_Type

   function "=" (Left, Right : Document_Type) return Boolean;
   --  Checks that both Left and Right have the same id.
   --  TODO: Confirm that both Left.Bare_Document and Right.Bare_Document
   --  should also point to the same object. If so, adapt this equality
   --  function accordingly.

   type Document_Array_Access is access all Document_Array;

   type Command_Kind is
     (Command_Align,
      Command_Break_Parent,
      Command_Cursor,
      Command_Fill,
      Command_Group,
      Command_If_Break,
      Command_Indent,
      Command_Indent_If_Break,
      Command_Label,
      Command_Line,
      Command_Line_Suffix,
      Command_Line_Suffix_Boundary,
      Command_Trim);

   type Command_Type (Kind : Command_Kind) is record
      case Kind is
         when Command_Align =>
            Align_Data     : Alignment_Data_Type;
            Align_Contents : Document_Type;

         when Command_Break_Parent =>
            null;

         when Command_Cursor =>
            Place_Holder : Symbol_Type;

         when Command_Fill =>
            Parts : Document_Type;

         when Command_Group =>
            Id              : Symbol_Type;
            Group_Contents  : Document_Type;
            Break           : Boolean;
            Expanded_States : Document_Type;

         when Command_If_Break =>
            If_Break_Group_Id : Symbol_Type;
            Break_Contents    : Document_Type;
            Flat_Contents     : Document_Type;

         when Command_Indent =>
            Indent_Contents : Document_Type;

         when Command_Indent_If_Break =>
            Indent_If_Break_Contents : Document_Type;
            Indent_If_Break_Group_Id : Symbol_Type;
            Negate                   : Boolean := False;

         when Command_Label =>
            Text           : Prettier_String;
            Label_Contents : Document_Type;

         when Command_Line =>
            Literal : Boolean;
            Soft    : Boolean;
            Hard    : Boolean;

         when Command_Line_Suffix =>
            Line_Suffix_Contents : Document_Type;

         when Command_Line_Suffix_Boundary =>
            null;

         when Command_Trim =>
            null;
      end case;
   end record;

   type Command_Access is access all Command_Type;

   type Document_Kind is (Document_Text, Document_List, Document_Command);

   type Bare_Document_Record (Kind : Document_Kind) is record
      Id : Natural;
      case Kind is
         when Document_Text =>
            Text : Prettier_String;
         when Document_List =>
            List : Document_Array_Access;
         when Document_Command =>
            --  TODO: We need an access here because Propagate_Breaks relies
            --  on a vector of mutable Command_Type objects. Investigate if
            --  there's a way of making these aliased and using a vector
            --  of an access to a Command_Type object.
            Command : Command_Access;
      end case;
   end record;

   type Bare_Document_Access is access all Bare_Document_Record;

   type Document_Type is record
      Bare_Document : Bare_Document_Access := null;
   end record;

   No_Document : constant Document_Type := (Bare_Document =>  null);

   type Symbol_Type is new Natural;

   No_Symbol : constant Symbol_Type := Symbol_Type'First;

end Prettier_Ada.Documents;
