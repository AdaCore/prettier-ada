--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with GNATCOLL.JSON;
with Ada.Containers;
with Ada.Containers.Hashed_Maps;

package body Prettier_Ada.Documents.Json is

   ---------------
   -- Serialize --
   ---------------

   function Serialize
     (Document : Document_Type)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      pragma Unreferenced (Document);

   begin
      --  TODO: Implement this

      return (raise Program_Error);
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

         ----------------------
         -- To_Document_Text --
         ----------------------

         function To_Document_Text
           (Json : GNATCOLL.JSON.JSON_Value;
            Id   : Natural)
            return Document_Type
         is
         begin
            return
              Document_Type'
                (Bare_Document =>
                   new Bare_Document_Record'
                         (Kind => Document_Text,
                          Id   => Id,
                          Text => Get (Json)));
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
            Documents : constant Document_Array_Access :=
              new Document_Array (1 .. Length);

         begin
            for J in 1 .. Length loop
               Documents (J) := To_Document_Type (Get (Elements, J));
            end loop;

            return
              Document_Type'
                (Bare_Document =>
                   new Bare_Document_Record'
                         (Kind => Document_List,
                          Id   => Id,
                          List => Documents));
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

            Kind : constant Document_Kind := Document_Command;

         begin
            case Command_Text is
               when "align" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'(To_Command_Align (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when "breakParent" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'(To_Command_Break_Parent (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when "fill" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'(To_Command_Fill (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when "group" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'(To_Command_Group (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when "ifBreak" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'(To_Command_If_Break (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when "indent" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'(To_Command_Indent (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when "indentIfBreak" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'(To_Command_Indent_If_Break (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when "line" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'(To_Command_Line (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when "label" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'(To_Command_Label (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when "lineSuffix" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'(To_Command_Line_Suffix (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when "lineSuffixBoundary" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'
                             (To_Command_Line_Suffix_Boundary (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when "trim" =>
                  declare
                     Command : constant Command_Access :=
                       new Command_Type'(To_Command_Trim (Json));
                  begin
                     return
                       Document_Type'
                         (Bare_Document =>
                            new Bare_Document_Record'(Kind, Id, Command));
                  end;

               when others =>
                  --  TODO: Raise a better exception
                  raise Program_Error;
            end case;
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
                        elsif Command_Kind = "text" then Text
                        elsif Command_Kind = "width" then Width
                        else raise Program_Error)));
               Align_Data : constant Alignment_Data_Type :=
                 (case Align_Data_Kind is
                    when Dedent_To_Root =>
                      Alignment_Data_Type'(Kind => Dedent_To_Root),
                    when Dedent =>
                      Alignment_Data_Type'(Kind => Dedent),
                    when Root =>
                      Alignment_Data_Type'(Kind => Root),
                    when Text =>
                      Alignment_Data_Type'
                        (Kind => Text,
                         T    => Get (Json, "t")),
                    when Width =>
                      Alignment_Data_Type'
                        (Kind => Width,
                         N    => Get (Json, "n")),
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
         begin
            return
              Command_Type'
                (Kind           => Command_Label,
                 Text           => Get (Json, "text"),
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
            raise Program_Error;
      end case;
   end Deserialize;

end Prettier_Ada.Documents.Json;
