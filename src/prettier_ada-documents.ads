--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers;
with Ada.Strings.Unbounded;

limited private with Prettier_Ada.Documents.Implementation;

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

   function "=" (Left, Right : Document_Type) return Boolean;
   --  Checks that both Left and Right refer to the same document

   function Hash (Document : Document_Type) return Ada.Containers.Hash_Type;
   --  Casts the Document id as Hash_Type

   function To_Document_Type (Text : Wide_Wide_String) return Document_Type;
   --  Convert given string into virtual string

   function Format
     (Document : Document_Type;
      Options  : Format_Options_Type := Default_Format_Options)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Formats Document with the given Options

   type Symbol_Type is private;

   function Hash (Symbol : Symbol_Type) return Ada.Containers.Hash_Type;
   --  Casts Symbol_Type as Hash_Type

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

   type Bare_Document_Access is
     access all Prettier_Ada.Documents.Implementation.Bare_Document_Record;

   type Document_Type is record
      Bare_Document : Bare_Document_Access := null;
   end record;

   No_Document : constant Document_Type := (Bare_Document => null);

   type Symbol_Type is new Natural;

   No_Symbol : constant Symbol_Type := Symbol_Type'First;

end Prettier_Ada.Documents;
