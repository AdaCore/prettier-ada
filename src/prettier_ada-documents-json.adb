--
--  Copyright (C) 2023-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Assertions;
with Ada.Containers;
with Ada.Containers.Hashed_Maps;

with VSS.Strings.Conversions;
with VSS.Strings.Utilities;

with GNATCOLL.JSON;

with Prettier_Ada.Documents.Implementation;
use Prettier_Ada.Documents.Implementation;
with Prettier_Ada.Document_Vectors;
with Prettier_Ada.Document_Vector_Vectors;

package body Prettier_Ada.Documents.Json is
   subtype Document_Vector is Prettier_Ada.Document_Vectors.Vector;
   subtype Document_Table is Prettier_Ada.Document_Vector_Vectors.Vector;

   function Assert_Equal (Actual, Expected : Command_Kind) return Boolean;
   --  Returns True is Actual = Expected, else raises an Assertion_Error.

   -------------------
   --  Assert_Equal --
   -------------------

   function Assert_Equal (Actual, Expected : Command_Kind) return Boolean
   is
   begin
      Ada.Assertions.Assert
        (Actual = Expected,
         "Unexpected Command_Type. Actual: """
         & Actual'Image
         & """. Expected: """
         & Expected'Image
         & """.");

      return True;
   end Assert_Equal;

   function Wrap_Command
     (Command : Command_Access; Id : Natural) return Document_Type
   is (Ada.Finalization.Controlled with
       Bare_Document => new Bare_Document_Record'
                              (Document_Command, 1, Id, Command));
   --  Allocate a new document to wrap the given command

   function Serialize
     (Document : Document_Type) return GNATCOLL.JSON.JSON_Value;
   --  Serialize a document into a JSON representation

   ---------------
   -- Serialize --
   ---------------

   function Serialize
     (Document : Document_Type) return GNATCOLL.JSON.JSON_Value
   is
      use GNATCOLL.JSON;

      function From_Document (Document : Document_Type) return JSON_Value;
      --  Serialize a document

      function From_Document_List (List : Document_Vector) return JSON_Value;
      --  Serialize a document list

      function From_Document_Table (Table : Document_Table) return JSON_Value;
      --  Serialize a document table

      function From_Command (Command : Command_Type) return JSON_Value;
      --  Serialize a command

      -------------------
      -- From_Document --
      -------------------

      function From_Document (Document : Document_Type) return JSON_Value is
         D : constant access Bare_Document_Record := Document.Bare_Document;
      begin
         if D = null then
            return Create;
         end if;

         return Result : constant JSON_Value := Create_Object do
            Result.Set_Field ("id", D.Id);
            case D.Kind is
               when Document_Text =>
                  Result.Set_Field ("kind", "text");
                  Result.Set_Field
                    ("text",
                     VSS.Strings.Conversions.To_UTF_8_String (D.Text.Text));

               when Document_List =>
                  Result.Set_Field ("kind", "list");
                  Result.Set_Field ("list", From_Document_List (D.List));

               when Document_Command =>
                  Result.Set_Field ("kind", "command");
                  Result.Set_Field ("command", From_Command (D.Command.all));
            end case;
         end return;
      end From_Document;

      ------------------------
      -- From_Document_List --
      ------------------------

      function From_Document_List (List : Document_Vector) return JSON_Value
      is
         Elements : JSON_Array;
      begin
         for D of List loop
            Append (Elements, From_Document (D));
         end loop;
         return Create (Elements);
      end From_Document_List;

      -------------------------
      -- From_Document_Table --
      -------------------------

      function From_Document_Table (Table : Document_Table) return JSON_Value
      is
         Elements : JSON_Array;
      begin
         for Row of Table loop
            Append (Elements, From_Document_List (Row));
         end loop;
         return Create (Elements);
      end From_Document_Table;

      ------------------
      -- From_Command --
      ------------------

      function From_Command (Command : Command_Type) return JSON_Value is
         Result : constant JSON_Value := Create_Object;
      begin
         case Command.Kind is
            when Command_Align =>
               Result.Set_Field ("command", "align");

               declare
                  Data : JSON_Value;
               begin
                  case Command.Align_Data.Kind is
                     when Width =>
                        Data := Create_Object;
                        Data.Set_Field ("kind", "width");
                        Data.Set_Field ("n", Command.Align_Data.N);

                     when Text =>
                        Data := Create_Object;
                        Data.Set_Field ("kind", "text");
                        Data.Set_Field
                          ("t",
                           Ada.Strings.Unbounded.To_String
                             (Command.Align_Data.T));

                     when Continuation_Line_Indent =>
                        Data := Create_Object;
                        Data.Set_Field ("kind", "continuationLineIndent");

                     when Dedent =>
                        Data := Create_Object;
                        Data.Set_Field ("kind", "dedent");

                     when Dedent_To_Root =>
                        Data := Create_Object;
                        Data.Set_Field ("kind", "dedentToRoot");

                     when Root =>
                        Data := Create_Object;
                        Data.Set_Field ("kind", "root");

                     when Inner_Root =>
                        Data := Create_Object;
                        Data.Set_Field ("kind", "innerRoot");

                     when None =>
                        Data := Create;
                  end case;
                  Result.Set_Field ("alignData", Data);
               end;

               Result.Set_Field
                 ("alignContents", From_Document (Command.Align_Contents));

            when Command_Break_Parent =>
               Result.Set_Field ("command", "breakParent");

            when Command_Cursor =>
               Result.Set_Field ("command", "cursor");
               Result.Set_Field
                 ("placeHolder", Natural (Command.Place_Holder));

            when Command_Fill =>
               Result.Set_Field ("command", "fill");
               Result.Set_Field ("parts", From_Document (Command.Parts));

            when Command_Group =>
               Result.Set_Field ("command", "group");
               Result.Set_Field ("id", Natural (Command.Id));
               Result.Set_Field
                 ("groupContents", From_Document (Command.Group_Contents));
               Result.Set_Field ("break", Command.Break);
               Result.Set_Field
                 ("expandedStates", From_Document (Command.Expanded_States));

            when Command_If_Break =>
               Result.Set_Field ("command", "ifBreak");
               Result.Set_Field
                 ("ifBreakGroupId", Natural (Command.If_Break_Group_Id));
               Result.Set_Field
                 ("breakContents", From_Document (Command.Break_Contents));
               Result.Set_Field
                 ("flatContents", From_Document (Command.Flat_Contents));

            when Command_Indent =>
               Result.Set_Field ("command", "indent");
               Result.Set_Field
                 ("indentContents", From_Document (Command.Indent_Contents));

            when Command_Indent_If_Break =>
               Result.Set_Field ("command", "indentIfBreak");
               Result.Set_Field
                 ("indentIfBreakContents",
                  From_Document (Command.Indent_If_Break_Contents));
               Result.Set_Field
                 ("indentIfBreakGroupId",
                  Natural (Command.Indent_If_Break_Group_Id));
               Result.Set_Field ("negate", Command.Negate);

            when Command_Label =>
               Result.Set_Field ("command", "label");
               Result.Set_Field
                 ("text",
                  VSS.Strings.Conversions.To_UTF_8_String (Command.Text.Text));
               Result.Set_Field
                 ("labelContents", From_Document (Command.Label_Contents));

            when Command_Line =>
               Result.Set_Field ("command", "line");
               Result.Set_Field ("literal", Command.Literal);
               Result.Set_Field ("soft", Command.Soft);
               Result.Set_Field ("hard", Command.Hard);

            when Command_Line_Suffix =>
               Result.Set_Field ("command", "lineSuffix");
               Result.Set_Field
                 ("lineSuffixContents",
                  From_Document (Command.Line_Suffix_Contents));

            when Command_Line_Suffix_Boundary =>
               Result.Set_Field ("command", "lineSuffixBoundary");

            when Command_Trim =>
               Result.Set_Field ("command", "trim");

            when Command_Alignment_Table =>
               Result.Set_Field ("command", "alignmentTable");
               Result.Set_Field
                 ("elements",
                  From_Document_Table (Command.Alignment_Table_Elements));
               Result.Set_Field
                 ("separators",
                  From_Document_Table (Command.Alignment_Table_Separators));
               Result.Set_Field
                 ("mustBreak", Command.Alignment_Table_Must_Break);
               Result.Set_Field
                 ("breakParents", Command.Break_Parents);

            when Command_Alignment_Table_Separator =>
               Result.Set_Field ("command", "alignmentTableSeparator");
               Result.Set_Field
                 ("text",
                  VSS.Strings.Conversions.To_UTF_8_String
                    (Command.Alignment_Table_Separator_Text.Text));

            when Command_Alignment_Table_End =>
               null; -- inner command that should not be serialized
         end case;

         return Result;
      end From_Command;

   begin
      return From_Document (Document);
   end Serialize;

   function Serialize
     (Document : Document_Type)
      return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      return Serialize (Document).Write (Compact => False);
   end Serialize;

   -----------------
   -- Deserialize --
   -----------------

   function Deserialize (Document : String) return Document_Type
   is
      Read_Result : constant GNATCOLL.JSON.Read_Result :=
        GNATCOLL.JSON.Read
          (Ada.Strings.Unbounded.To_Unbounded_String (Document));

      function Hash (N : Natural) return Ada.Containers.Hash_Type is
         (Ada.Containers.Hash_Type (N));

      package Natural_To_Document_Type_Hashed_Maps is new
        Ada.Containers.Hashed_Maps
          (Natural, Document_Type, Hash, "=", "=");

      subtype Natural_To_Document_Type_Hashed_Map is
        Natural_To_Document_Type_Hashed_Maps.Map;

      Documents_Cache : Natural_To_Document_Type_Hashed_Map;

      function To_Document_Type
        (Json : GNATCOLL.JSON.JSON_Value)
         return Document_Type;
      --  TODO: Description

      ----------------------
      -- To_Document_Type --
      ----------------------

      function To_Document_Type
        (Json : GNATCOLL.JSON.JSON_Value)
         return Document_Type
      is
         use GNATCOLL.JSON;

         function To_Document_Text
           (Json : GNATCOLL.JSON.JSON_Value;
            Id   : Natural)
            return Document_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Document_List
           (Json : GNATCOLL.JSON.JSON_Value;
            Id   : Natural)
            return Document_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Document_Command
           (Json : GNATCOLL.JSON.JSON_Value;
            Id   : Natural)
            return Document_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Align
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Break_Parent
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Cursor
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Fill
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Group
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_If_Break
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Indent
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Indent_If_Break
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Line
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Label
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Line_Suffix
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Line_Suffix_Boundary
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Trim
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type;
         --  TODO: Add description
         --  TODO: Add Pre and Post contracts

         function To_Command_Alignment_Table
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         with Post => Assert_Equal
                        (To_Command_Alignment_Table'Result.Kind,
                         Command_Alignment_Table);
         --  Decodes Json as a Alignment_Table command

         function To_Command_Alignment_Table_Separator
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         with Post => Assert_Equal
                        (To_Command_Alignment_Table_Separator'Result.Kind,
                         Command_Alignment_Table_Separator);
         --  Decodes Json as a Alignment_Table_Separator command

         ----------------------
         -- To_Document_Text --
         ----------------------

         function To_Document_Text
           (Json : GNATCOLL.JSON.JSON_Value;
            Id   : Natural)
            return Document_Type
         is
            Text          : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                (UTF8_String'(Get (Json)));
            Display_Width : constant VSS.Strings.Display_Cell_Count :=
              VSS.Strings.Utilities.Display_Width (Text);

         begin
            return
              (Ada.Finalization.Controlled with
               Bare_Document =>
                 new Bare_Document_Record'
                       (Kind      => Document_Text,
                        Ref_Count => 1,
                        Id        => Id,
                        Text      => (Text, Display_Width)));
         end To_Document_Text;

         ----------------------
         -- To_Document_List --
         ----------------------

         function To_Document_List
           (Json : GNATCOLL.JSON.JSON_Value;
            Id   : Natural)
            return Document_Type
         is
            Elements  : constant JSON_Array := Get (Json);
            Length    : constant Natural :=
              GNATCOLL.JSON.Length (Elements);
            Documents : Document_Vector;

         begin
            for J in 1 .. Length loop
               Documents.Append (To_Document_Type (Get (Elements, J)));
            end loop;

            return
              (Ada.Finalization.Controlled with
               Bare_Document =>
                 new Bare_Document_Record'
                       (Kind      => Document_List,
                        Ref_Count => 1,
                        Id        => Id,
                        List      => Documents));
         end To_Document_List;

         -------------------------
         -- To_Document_Command --
         -------------------------

         function To_Document_Command
           (Json : GNATCOLL.JSON.JSON_Value;
            Id   : Natural)
            return Document_Type
         is
            Command_Text : constant UTF8_String :=  Get (Json, "command");

         begin
            if Command_Text = "align" then
               return
                 Wrap_Command (new Command_Type'(To_Command_Align (Json)), Id);

            elsif Command_Text = "breakParent" then
               return
                 Wrap_Command
                   (new Command_Type'(To_Command_Break_Parent (Json)), Id);

            elsif Command_Text = "cursor" then
               return
                 Wrap_Command (new Command_Type'(To_Cursor (Json)), Id);

            elsif Command_Text = "fill" then
               return
                 Wrap_Command (new Command_Type'(To_Command_Fill (Json)), Id);

            elsif Command_Text = "group" then
               return
                 Wrap_Command (new Command_Type'(To_Command_Group (Json)), Id);

            elsif Command_Text = "ifBreak" then
               return
                 Wrap_Command
                   (new Command_Type'(To_Command_If_Break (Json)), Id);

            elsif Command_Text = "indent" then
               return
                 Wrap_Command
                   (new Command_Type'(To_Command_Indent (Json)), Id);

            elsif Command_Text = "indentIfBreak" then
               return
                 Wrap_Command
                   (new Command_Type'(To_Command_Indent_If_Break (Json)), Id);

            elsif Command_Text = "line" then
               return
                 Wrap_Command (new Command_Type'(To_Command_Line (Json)), Id);

            elsif Command_Text = "label" then
               return
                 Wrap_Command (new Command_Type'(To_Command_Label (Json)), Id);

            elsif Command_Text = "lineSuffix" then
               return
                 Wrap_Command
                   (new Command_Type'(To_Command_Line_Suffix (Json)), Id);

            elsif Command_Text = "lineSuffixBoundary" then
               return
                 Wrap_Command
                   (new Command_Type'(To_Command_Line_Suffix_Boundary (Json)),
                    Id);

            elsif Command_Text = "trim" then
               return
                 Wrap_Command (new Command_Type'(To_Command_Trim (Json)), Id);

            elsif Command_Text = "alignmentTable" then
               return
                 Wrap_Command
                   (new Command_Type'(To_Command_Alignment_Table (Json)), Id);

            elsif Command_Text = "alignmentTableSeparator" then
               return
                 Wrap_Command
                   (new Command_Type'
                          (To_Command_Alignment_Table_Separator (Json)),
                    Id);

            else
               --  TODO: Raise a better exception
               raise Program_Error;
            end if;
         end To_Document_Command;

         ----------------------
         -- To_Command_Align --
         ----------------------

         function To_Command_Align
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            function To_Align_Data
              (Json : GNATCOLL.JSON.JSON_Value)
               return Alignment_Data_Type;
            --  TODO: Add description

            -------------------
            -- To_Align_Data --
            -------------------

            function To_Align_Data
              (Json : GNATCOLL.JSON.JSON_Value)
               return Alignment_Data_Type
            is
               Align_Data_Kind : constant Align_Kind_Type :=
                 (if Kind (Json) = JSON_Null_Type then
                    None
                  else
                    (declare Command_Kind : constant String :=
                       Get (Json, "kind");
                     begin
                       (if Command_Kind = "dedentToRoot" then Dedent_To_Root
                        elsif Command_Kind = "dedent" then Dedent
                        elsif Command_Kind = "root" then Root
                        elsif Command_Kind = "innerRoot" then Inner_Root
                        elsif Command_Kind = "text" then Text
                        elsif Command_Kind = "width" then Width
                        elsif Command_Kind = "continuationLineIndent"
                          then Continuation_Line_Indent
                        else raise Program_Error)));
               Align_Data : constant Alignment_Data_Type :=
                 (case Align_Data_Kind is
                    when Dedent_To_Root =>
                      Alignment_Data_Type'(Kind => Dedent_To_Root),
                    when Dedent =>
                      Alignment_Data_Type'(Kind => Dedent),
                    when Root =>
                      Alignment_Data_Type'(Kind => Root),
                    when Inner_Root =>
                      Alignment_Data_Type'(Kind => Inner_Root),
                    when Text =>
                      Alignment_Data_Type'
                        (Kind => Text,
                         T    => UTF8_Unbounded_String'(Get (Json, "t"))),
                    when Width =>
                      Alignment_Data_Type'
                        (Kind => Width,
                         N    => Get (Json, "n")),
                    when Continuation_Line_Indent =>
                      Alignment_Data_Type'(Kind => Continuation_Line_Indent),
                    when None =>
                      Alignment_Data_Type'(Kind => None));
            begin
               return Align_Data;
            end To_Align_Data;

            Kind           : constant Command_Kind := Command_Align;
            Align_Data     : constant Alignment_Data_Type :=
              To_Align_Data (Get (Json, "alignData"));
            Align_Contents : constant Document_Type :=
              To_Document_Type (Get (Json, "alignContents"));

         begin
            return Command_Type'(Kind, Align_Data, Align_Contents);
         end To_Command_Align;

         -----------------------------
         -- To_Command_Break_Parent --
         -----------------------------

         function To_Command_Break_Parent
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            return Command_Type'(Kind => Command_Break_Parent);
         end To_Command_Break_Parent;

         ---------------------
         -- To_Command_Fill --
         ---------------------

         function To_Cursor
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            Place_Holder : constant Natural := Get (Json, "placeHolder");
         begin
            return
              Command_Type'
                (Kind         => Command_Cursor,
                 Place_Holder => Symbol_Type (Place_Holder));
         end To_Cursor;

         ---------------------
         -- To_Command_Fill --
         ---------------------

         function To_Command_Fill
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
         begin
            return
              Command_Type'
                (Kind => Command_Fill,
                 Parts => To_Document_Type (Get (Json, "parts")));
         end To_Command_Fill;

         ----------------------
         -- To_Command_Group --
         ----------------------

         function To_Command_Group
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            Id : constant Natural := Get (Json, "id");

         begin
            return
              Command_Type'
                (Kind            => Command_Group,
                 Id              => Symbol_Type (Id),
                 Break           => Get (Json, "break"),
                 Group_Contents  =>
                   To_Document_Type (Get (Json, "groupContents")),
                 Expanded_States =>
                   To_Document_Type (Get (Json, "expandedStates")));
         end To_Command_Group;

         -------------------------
         -- To_Command_If_Break --
         -------------------------

         function To_Command_If_Break
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            If_Break_Group_Id : constant Natural :=
              Get (Json, "ifBreakGroupId");

         begin
            return
              Command_Type'
                (Kind              => Command_If_Break,
                 If_Break_Group_Id => Symbol_Type (If_Break_Group_Id),
                 Break_Contents    =>
                   To_Document_Type (Get (Json, "breakContents")),
                 Flat_Contents     =>
                   To_Document_Type (Get (Json, "flatContents")));
         end To_Command_If_Break;

         -----------------------
         -- To_Command_Indent --
         -----------------------

         function To_Command_Indent
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
         begin
            return
              Command_Type'
                (Kind            => Command_Indent,
                 Indent_Contents =>
                   To_Document_Type (Get (Json, "indentContents")));
         end To_Command_Indent;

         --------------------------------
         -- To_Command_Indent_If_Break --
         --------------------------------

         function To_Command_Indent_If_Break
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            Indent_If_Break_Group_Id : constant Natural :=
              Get (Json, "indentIfBreakGroupId");

         begin
            return
              Command_Type'
                (Kind                     => Command_Indent_If_Break,
                 Indent_If_Break_Contents =>
                   To_Document_Type (Get (Json, "indentIfBreakContents")),
                 Indent_If_Break_Group_Id =>
                   Symbol_Type (Indent_If_Break_Group_Id),
                 Negate => Get (Json, "negate"));
         end To_Command_Indent_If_Break;

         ----------------------
         -- To_Command_Label --
         ----------------------

         function To_Command_Label
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            Text          : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                (UTF8_String'(Get (Json, "text")));
            Display_Width : constant VSS.Strings.Display_Cell_Count :=
              VSS.Strings.Utilities.Display_Width (Text);

         begin
            return
              Command_Type'
                (Kind           => Command_Label,
                 Text           => (Text, Display_Width),
                 Label_Contents =>
                   To_Document_Type (Get (Json, "labelContents")));
         end To_Command_Label;

         ---------------------
         -- To_Command_Line --
         ---------------------

         function To_Command_Line
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
         begin
            return
              Command_Type'
                (Kind    => Command_Line,
                 Literal => Get (Json, "literal"),
                 Soft    => Get (Json, "soft"),
                 Hard    => Get (Json, "hard"));
         end To_Command_Line;

         ----------------------------
         -- To_Command_Line_Suffix --
         ----------------------------

         function To_Command_Line_Suffix
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
         begin
            return
              Command_Type'
                (Kind                 => Command_Line_Suffix,
                 Line_Suffix_Contents =>
                   To_Document_Type (Get (Json, "lineSuffixContents")));
         end To_Command_Line_Suffix;

         -------------------------------------
         -- To_Command_Line_Suffix_Boundary --
         -------------------------------------

         function To_Command_Line_Suffix_Boundary
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            return
              Command_Type'
                (Kind => Command_Line_Suffix_Boundary);
         end To_Command_Line_Suffix_Boundary;

         ---------------------
         -- To_Command_Trim --
         ---------------------

         function To_Command_Trim
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            pragma Unreferenced (Json);
         begin
            return (Kind => Command_Trim);
         end To_Command_Trim;

         --------------------------------
         -- To_Command_Alignment_Table --
         --------------------------------

         function To_Command_Alignment_Table
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            function To_Document_Table
              (Json_Table : GNATCOLL.JSON.JSON_Value)
               return Document_Table;
            --  TODO: Add description
            --  TODO: Add Pre and Post contracts

            -----------------------
            -- To_Document_Table --
            -----------------------

            function To_Document_Table
              (Json_Table : GNATCOLL.JSON.JSON_Value)
               return Document_Table
            is
               Table : Document_Table;

               Rows       : constant JSON_Array := Get (Json_Table);
               Total_Rows : constant Natural    := GNATCOLL.JSON.Length (Rows);

            begin
               for Row_Index in 1 .. Total_Rows loop
                  declare
                     Current_Row   : constant JSON_Array :=
                       Get (Get (Rows, Row_Index));
                     Total_Columns : constant Natural    :=
                       GNATCOLL.JSON.Length (Current_Row);

                     Row : Document_Vector;

                  begin
                     for Column_Index in 1 .. Total_Columns loop
                        Row.Append
                          (To_Document_Type (Get (Current_Row, Column_Index)));
                     end loop;

                     Table.Append (Row);
                  end;
               end loop;

               return Table;
            end To_Document_Table;

         begin
            return
              Command_Type'
                (Kind                       => Command_Alignment_Table,
                 Alignment_Table_Elements   =>
                   To_Document_Table (Get (Json, "elements")),
                 Alignment_Table_Separators =>
                   To_Document_Table (Get (Json, "separators")),
                 Alignment_Table_Must_Break => Get (Json, "mustBreak"),
                 Break_Parents              => Get (Json, "breakParents"));
         end To_Command_Alignment_Table;

         --------------------------------
         -- To_Command_Alignment_Table --
         --------------------------------

         function To_Command_Alignment_Table_Separator
           (Json : GNATCOLL.JSON.JSON_Value)
            return Command_Type
         is
            Text          : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                (UTF8_String'(Get (Json, "text")));
            Display_Width : constant VSS.Strings.Display_Cell_Count :=
              VSS.Strings.Utilities.Display_Width (Text);
         begin
            return
              Command_Type'
                (Kind           => Command_Alignment_Table_Separator,
                 Alignment_Table_Separator_Text => (Text, Display_Width));
         end To_Command_Alignment_Table_Separator;

      begin
         case Kind (Json) is
            when JSON_Boolean_Type
                 | JSON_Int_Type
                 | JSON_Float_Type
                 | JSON_Array_Type
                 | JSON_String_Type =>
               raise Program_Error;

            when JSON_Null_Type =>
               return No_Document;

            when JSON_Object_Type =>
               declare
                  Kind : constant String := Get (Json, "kind");
                  Id   : constant Natural := Get (Json, "id");

               begin
                  if Documents_Cache.Contains (Id) then
                     return Documents_Cache.Element (Id);
                  end if;

                  return New_Document : constant Document_Type :=
                    (if Kind = "text" then
                       To_Document_Text (Get (Json, "text"), Id)
                     elsif Kind = "list" then
                       To_Document_List (Get (Json, "list"), Id)
                     elsif Kind = "command" then
                       To_Document_Command (Get (Json, "command"), Id)
                     else
                       raise Program_Error)
                  do
                     Documents_Cache.Insert (Id, New_Document);
                  end return;
               end;

         end case;
      end To_Document_Type;

   begin
      case Read_Result.Success is
         when True =>
            return To_Document_Type (Read_Result.Value);
         when False =>
            --  TODO: Gracefully handle this
            raise Program_Error
              with GNATCOLL.JSON.Format_Parsing_Error (Read_Result.Error);
      end case;
   end Deserialize;

end Prettier_Ada.Documents.Json;
