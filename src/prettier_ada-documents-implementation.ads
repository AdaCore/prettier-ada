--
--  Copyright (C) 2023-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers;
with Ada.Strings.Unbounded;

with Prettier_Ada.Document_Vectors;

with VSS.Strings;

--  This package provides the Document_Type definition (equivalent to
--  Prettier's Doc type) and format function.
--
--  To create a Document_Type, use the Prettier_Ada.Documents.Builders package.

private package Prettier_Ada.Documents.Implementation is

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

   function New_Symbol return Symbol_Type;
   --  Returns a new Symbol_Type, used to uniquely identify parent
   --  Command_Group documents.

   function Hash (Symbol : Symbol_Type) return Ada.Containers.Hash_Type;
   --  Casts Symbol_Type as Hash_Type

   function New_Document_Id return Natural;
   --  Creates an unique id to be used when building a document

   procedure Reset_Document_Id;
   --  Each Document_Type built by the builders found on this packages will
   --  have an unique ID. When building a Document_Type for several sources,
   --  this function can be called to reset the Document_Type id. This is
   --  important to do when the user is building several documents and then
   --  formatting them, instead of formatting right after creating a document.
   --  This function is automatically called when
   --  Prettier_Ada.Documents.Format finishes.
   --
   --  TODO: Address how this global mutable state affects multi-threaded
   --  programs.

   type Prettier_String is
     record
       Text          : VSS.Strings.Virtual_String;
       Display_Width : VSS.Strings.Display_Cell_Count := 0;
       --  Display_Width is used to cache Text's display width since it's a
       --  costly computation that needs to be computed often.
     end record;

   Empty_Prettier_String : constant Prettier_String :=
     (VSS.Strings.Empty_Virtual_String, 0);

   function To_Prettier_String
     (Text : Ada.Strings.Unbounded.Unbounded_String) return Prettier_String;
   --  Converts an Unbounded_String into a Prettier_String

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
      Ref_Count : Natural;
      Id        : Natural;
      case Kind is
         when Document_Text =>
            Text : Prettier_String;
         when Document_List =>
            List : Prettier_Ada.Document_Vectors.Vector;
         when Document_Command =>
            --  TODO: We need an access here because Propagate_Breaks relies
            --  on a vector of mutable Command_Type objects. Investigate if
            --  there's a way of making these aliased and using a vector
            --  of an access to a Command_Type object.
            Command : Command_Access;
      end case;
   end record;

end Prettier_Ada.Documents.Implementation;
