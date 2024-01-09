--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Prettier_Ada.Document_Vectors;

--  This package provides functions to build a Document_Type.
--  The API mimics the original Prettier builders API with the following
--  exceptions:
--  - To create a string Document, instead of using a string literal, use
--    the Text function.
--  - To create an array of Documents, instead of using the square bracket
--    notation, use the List function. This converts a Document_Vector into
--    a Document_Type.

package Prettier_Ada.Documents.Builders is

   subtype Document_Vector is Prettier_Ada.Document_Vectors.Vector;

   No_Document : constant Document_Type;

   No_Symbol : constant Symbol_Type;

   procedure Reset_Document_Id;
   --  Each Document_Type built by the builders found on this packages will
   --  have an unique ID. When building a Document_Type for several sources,
   --  this function can be called to reset the Document_Type id. This is
   --  important to do when the user is building several documents and then
   --  formatting them, instead of formatting right after creating a document.
   --  This function is automatically called when
   --  Prettier_Ada.Documents.Format finishes.

   function Text
     (T : Ada.Strings.Unbounded.Unbounded_String)
      return Document_Type;
   --  Convert text into a Document_Type object. T is expected to be UTF-8
   --  encoded.

   function List
     (Documents : Document_Vector)
      return Document_Type;
   --  Convert an array of Document_Type objects into a Document_Type object

   function Align
     (Data     : Alignment_Data_Type;
      Contents : Document_Type)
      return Document_Type;
   --  Creates a new Align Document Command

   function Align
     (Data     : Alignment_Data_Type;
      Contents : Document_Vector)
      return Document_Type;
   --  Creates a new Align Document Command

   function Break_Parent return Document_Type;
   --  Creates a new Break_Parent Document Command

   function Cursor return Document_Type;
   --  Creates a new Cursor Document Command

   function Fill
     (Parts : Document_Type)
      return Document_Type;
   --  Creates a new Fill Document Command

   function Fill
     (Parts : Document_Vector)
      return Document_Type;
   --  Creates a new Fill Document Command

   type Group_Options_Type is record
      Should_Break    : Boolean;
      Id              : Symbol_Type;
      Expanded_States : Document_Type;
   end record;

   No_Group_Options : constant Group_Options_Type;

   function Group
     (Documents : Document_Type;
      Options   : Group_Options_Type := No_Group_Options)
      return Document_Type;
   --  Creates a new Group Document Command

   function Group
     (Documents : Document_Vector;
      Options   : Group_Options_Type := No_Group_Options)
      return Document_Type;
   --  Creates a new Group Document Command

   type If_Break_Options_Type is record
      Group_Id : Symbol_Type;
   end record;

   No_If_Break_Options : constant If_Break_Options_Type;

   function If_Break
     (Break_Contents : Document_Type;
      Flat_Contents  : Document_Type := No_Document;
      Options        : If_Break_Options_Type := No_If_Break_Options)
      return Document_Type;
   --  Creates a new If_Break Document Command.
   --
   --  TODO: Investigate if Break_Contents and Flat_Contents must always be a
   --  Document_Vector. This is not possible to confirm from the Prettier
   --  API, so an analysis to the formatting algorithm must be done.

   function Indent (Contents : Document_Type) return Document_Type;
   --  Creates a new Indent Document Command.
   --
   --  TODO: Investigate if Contents always be a Document_Vector. This is
   --  not possible to confirm from the Prettier API, so an analysis to the
   --  formatting algorithm must be done.

   type Indent_If_Break_Options_Type is record
      Group_Id : Symbol_Type;
      Negate   : Boolean;
   end record;

   No_Indent_If_Break_Options : constant Indent_If_Break_Options_Type;

   function Indent_If_Break
     (Contents : Document_Type;
      Options : Indent_If_Break_Options_Type := No_Indent_If_Break_Options)
      return Document_Type;
   --  Creates a new Indent_If_Break Document Command

   function Label
     (Text     : Ada.Strings.Unbounded.Unbounded_String;
      Contents : Document_Type)
      return Document_Type;
   --  Creates a new Label Document Command

   function Line return Document_Type;
   --  Creates a new Line Document Command

   function Soft_Line return Document_Type;
   --  Creates a new soft Line Document Command

   function Hard_Line return Document_Type;
   --  Creates a new hard Line Document Command

   function Literal_Line return Document_Type;
   --  Creates a new literal Line Document Command

   function Hard_Line_Without_Break_Parent return Document_Type;
   --  Creates a new hard Line Document Command, without a Break_Parent
   --  Document_Type.

   function Literal_Line_Without_Break_Parent return Document_Type;
   --  Creates a new literal Line Document Command, without a Break_Parent
   --  Document_Type.

   function Line_Suffix (Contents : Document_Type) return Document_Type;
   --  Creates a new Line_Suffix Document Command

   function Line_Suffix_Boundary return Document_Type;
   --  Creates a new Line_Suffix_Boundary Document Command

   function Trim return Document_Type;
   --  Creates a new Trim Document Command

   function Join
     (Separator : Document_Type;
      Documents : Document_Vector)
      return Document_Type;
   --  Join an array of Documents with a Separator

private

   No_Document : constant Document_Type := Prettier_Ada.Documents.No_Document;

   No_Symbol : constant Symbol_Type := Prettier_Ada.Documents.No_Symbol;

   No_Group_Options : constant Group_Options_Type :=
     (Should_Break    => False,
      Id              => No_Symbol,
      Expanded_States => No_Document);

   No_Indent_If_Break_Options : constant Indent_If_Break_Options_Type :=
     (Group_Id => No_Symbol, Negate => False);

   No_If_Break_Options : constant If_Break_Options_Type :=
     (Group_Id => No_Symbol);

end Prettier_Ada.Documents.Builders;
