--
--  Copyright (C) 2023-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers;
private with Ada.Finalization;
with Ada.Strings.Unbounded;

limited private with Prettier_Ada.Documents.Implementation;

--  This package provides the Document_Type definition (equivalent to
--  Prettier's Doc type) and format function.
--  To create a Document_Type, use the Prettier_Ada.Documents.Builders package.

package Prettier_Ada.Documents is

   type Indentation_Kind is (Spaces, Tabs);

   type Indentation_Offset_Type is record
      Tabs   : Natural := 0;
      Spaces : Natural := 0;
   end record;
   --  This offset type models smart tabs. It allows two ways of indenting:
   --  - Tabs for indentation and spaces for alignment
   --  - Spaces for indentation and alignment

   type Indentation_Options_Type is record
      Kind         : Indentation_Kind := Spaces;
      Width        : Natural := 2;
      Continuation : Natural := 2;
      Offset       : Indentation_Offset_Type := (Tabs => 0, Spaces => 0);
   end record;

   type End_Of_Line_Kind is (LF, CR, CRLF);

   type Format_Options_Type is record
      Width       : Natural;
      Indentation : Indentation_Options_Type;
      End_Of_Line : End_Of_Line_Kind;
   end record;

   Default_Format_Options : constant Format_Options_Type :=
     (Width                    => 80,
      Indentation              => (Spaces, 2, 2, (0, 0)),
      End_Of_Line              => LF);

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

   No_Symbol : constant Symbol_Type;

   function Image (Symbol : Symbol_Type) return String;
   --  Return a human representation of a symbol, for debugging purposes

   function Hash (Symbol : Symbol_Type) return Ada.Containers.Hash_Type;
   --  Casts Symbol_Type as Hash_Type

   function New_Symbol return Symbol_Type;
   --  Returns a new Symbol_Type, used to uniquely identify parent
   --  Command_Group documents.

   --  TODO: Align_Data_Type is necessary for the the Document_Type and its
   --  builders but it does not fit well on the public part of this package.
   --  Try to make this private.

   type Align_Kind_Type is
     (Width,
      Text,
      Dedent,
      Dedent_To_Root,
      Root,
      Inner_Root,
      Continuation_Line_Indent,
      None);
   --  Width, Text, Dedent, Dedent_To_Root and Root are align kinds ported from
   --  and with the same behaviour as in Prettier.
   --
   --  None is added by this library just to make Alignment_Data_Type
   --  constrained. It is useless when used to create an Alignment_Data_Type
   --  object since it will result in no alignment being added.
   --
   --  Inner_Root is an extention added by this library.
   --
   --  It forces the indentation to be the line length right before formatting
   --  the Document built with the Prettier_Ada.Documents.Builder.Align.
   --  Any previous tab indentation added by the Indent command or Align Width
   --  kind command is kept. All others are ignored.
   --
   --  An example where this is needed is the following IfStmt where we want
   --  the DottedName suffix to be aligned based on the first name.
   --
   --  ```
   --  if AAAAA
   --       .BBBBBB
   --    and then CCCCC
   --               .DDDDD
   --  then
   --  ```
   --
   --  Continuation_Line_Indent is an extension added by this library. It's
   --  similar to Text, where T is a string with spaces. The amount of spaces
   --  is determined dynamically by the Format_Options_Type.Continuation_Line
   --  that is being used to format the document.

   type Alignment_Data_Type (Kind : Align_Kind_Type := None) is record
      case Kind is
         when Width =>
            N : Natural; -- Number of spaces or tabs
         when Text =>
            T : Ada.Strings.Unbounded.Unbounded_String;
         when Dedent
            | Dedent_To_Root
            | Root
            | Inner_Root
            | Continuation_Line_Indent
            | None
         =>
            null;
      end case;
   end record;

private

   type Bare_Document_Access is
     access all Prettier_Ada.Documents.Implementation.Bare_Document_Record;

   type Document_Type is new Ada.Finalization.Controlled with record
      Bare_Document : Bare_Document_Access := null;
   end record;

   overriding procedure Adjust (Self : in out Document_Type);
   overriding procedure Finalize (Self : in out Document_Type);

   No_Document : constant Document_Type :=
     (Ada.Finalization.Controlled with Bare_Document => null);

   type Symbol_Type is new Natural;

   No_Symbol : constant Symbol_Type := Symbol_Type'First;

end Prettier_Ada.Documents;
