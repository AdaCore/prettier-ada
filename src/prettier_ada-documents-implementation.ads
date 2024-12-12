--
--  Copyright (C) 2023-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers;
with Ada.Strings.Unbounded;

with Prettier_Ada.Document_Vectors;
with Prettier_Ada.Document_Vector_Vectors;

with VSS.Strings;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;
private with Ada.Unchecked_Deallocation;

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

   subtype Document_Vector is Prettier_Ada.Document_Vectors.Vector;

   subtype Document_Table is Prettier_Ada.Document_Vector_Vectors.Vector;

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
      Command_Trim,
      Command_Alignment_Table,
      Command_Alignment_Table_Separator,
      Command_Alignment_Table_End);

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

         when Command_Alignment_Table =>
            Alignment_Table_Elements   : Document_Table;
            Alignment_Table_Separators : Document_Table;
            Alignment_Table_Must_Break : Boolean;
            --  If True, adds a Hardline between table rows. If the table has
            --  more than one row, also breaks parents (and implies that
            --  Break_Parents = True).

            Break_Parents              : Boolean;
            --  Flag used by the Propagate_Breaks procedure. If True, breaks
            --  parent documents. This is initially set by the
            --  Alignment_Table builder based on the Must_Break value and rows
            --  count. The Propagate_Breaks procedure can change this value to
            --  True if it detects Break_Parent commands inside the table rows.

         when Command_Alignment_Table_Separator =>
            Alignment_Table_Separator_Text : Prettier_String;

         when Command_Alignment_Table_End =>
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

   function Wrap_Command
     (Command : Command_Access) return Document_Type
   is (Ada.Finalization.Controlled with
       Bare_Document => new Bare_Document_Record'
                              (Document_Command, 1, New_Document_Id, Command));
   --  Allocate a new document to wrap the given command

private

   ------------------
   --  Indentation --
   ------------------

   type Indentation_Data_Kind is
     (Indent, String_Align, Number_Align, Dedent, Inner_Root);

   type Indentation_Data_Record (Kind : Indentation_Data_Kind := Indent) is
   record
      case Kind is
         when Indent | Dedent =>
            null;

         when Inner_Root =>
            Margin : Natural;

         when String_Align =>
            Text : Prettier_String;

         when Number_Align =>
            Width : Natural;
      end case;
   end record;

   package Indentation_Data_Vectors is new
     Ada.Containers.Vectors (Natural, Indentation_Data_Record);

   subtype Indentation_Data_Vector is Indentation_Data_Vectors.Vector;

   type Indentation_Queue_Record;

   type Indentation_Queue_Access is access Indentation_Queue_Record;

   type Indentation_Queue_Record is record
      Value : Prettier_String;
      Queue : Indentation_Data_Vector;
      Root  : Indentation_Queue_Access;
   end record;

   procedure Free is new
     Ada.Unchecked_Deallocation
       (Indentation_Queue_Record,
        Indentation_Queue_Access);

   package Indentation_Queue_Vectors is new
     Ada.Containers.Vectors (Positive, Indentation_Queue_Access);

   type Indentation_Queue_Pool_Record is tagged record
      Pool : Indentation_Queue_Vectors.Vector;
   end record;

   procedure Free_Pool (Self : in out Indentation_Queue_Pool_Record);
   --  Free all allocated Indentation_Queue_Record objects

   function Root_Indent
     (Self    : in out Indentation_Queue_Pool_Record;
      Options : Indentation_Options_Type) return Indentation_Queue_Access;
   --  Creates an initial Indentation_Queue_Record with Options.Offset as
   --  indentation offset.

   function Make_Indentation
     (Self    : in out Indentation_Queue_Pool_Record;
      From    : Indentation_Queue_Access;
      Options : Indentation_Options_Type) return Indentation_Queue_Access;
   --  Copies From and adds a new indentation of kind Indent based on Options.

   function Make_Align
     (Self                : in out Indentation_Queue_Pool_Record;
      From                : Indentation_Queue_Access;
      Align_Data          : Alignment_Data_Type;
      Options             : Format_Options_Type;
      Current_Line_Length : Natural) return Indentation_Queue_Access;
   --  Copies From and adds a new alignment of kind based on Align_Data and
   --  Options.

   function Generate_Indentation
     (Self    : in out Indentation_Queue_Pool_Record;
      From    : Indentation_Queue_Access;
      Data    : Indentation_Data_Record;
      Options : Indentation_Options_Type) return Indentation_Queue_Access;
   --  Copies From and adds a new alignment of kind based on Data and Options

   type Indentation_Queue_Pool_Access is access Indentation_Queue_Pool_Record;

   procedure Free is new
     Ada.Unchecked_Deallocation
       (Indentation_Queue_Pool_Record,
        Indentation_Queue_Pool_Access);

   -------------------
   -- Print Command --
   -------------------

   type Mode_Kind is (Mode_Break, Mode_Flat, None);

   type Print_Command_Record is record
      Indentation : Indentation_Queue_Access;
      Mode        : Mode_Kind;
      Document    : Document_Type;
   end record;

   package Print_Command_Vectors is new
     Ada.Containers.Vectors (Positive, Print_Command_Record);

   subtype Print_Command_Vector is Print_Command_Vectors.Vector;

   type Print_Command_Vector_Access is
     access all Print_Command_Vector;

   procedure Free is new
     Ada.Unchecked_Deallocation
       (Print_Command_Vector, Print_Command_Vector_Access);

   -------------------
   --  Format State --
   -------------------

   package Symbol_To_Mode_Maps is new
     Ada.Containers.Hashed_Maps (Symbol_Type, Mode_Kind, Hash, "=");

   subtype Symbol_To_Mode_Map is Symbol_To_Mode_Maps.Map;

   type Format_State_Record is tagged record
      Result                 : Prettier_String;
      Current_Line_Length    : Natural;
      Indentation_Queue_Pool : Indentation_Queue_Pool_Access;
      Print_Commands         : Print_Command_Vector_Access;
      Line_Suffix            : Print_Command_Vector_Access;
      Group_Mode_Map         : Symbol_To_Mode_Map;
      Printed_Cursor_Count   : Natural;
      Should_Remeasure       : Boolean;
      Last_Was_Hardline      : Boolean;
      --  Flag indicating if the last text added to Result was a hardline
   end record;

   procedure Format
     (Format_State     : in out Format_State_Record;
      Format_Options   : Format_Options_Type);
   --  Resume the formatting operations on Format_State.
   --  Formats according to Format_Options.

   procedure Clear (Self : in out Format_State_Record);
   --  Free all allocated resources

end Prettier_Ada.Documents.Implementation;
