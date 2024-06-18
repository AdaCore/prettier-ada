--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with GNAT.Strings;

with GNATCOLL.VFS;
with GNATCOLL.Traces;

with Document_Formatter.Command_Line;

with Prettier_Ada.Documents;
with Prettier_Ada.Documents.Builders;
with Prettier_Ada.Documents.Json;

procedure Document_Formatter.Main is

   function Build_Format_Options
     return Prettier_Ada.Documents.Format_Options_Type;
   --  TODO: Description

   function Detect_End_Of_Line return Prettier_Ada.Documents.End_Of_Line_Kind;
   --  TODO: Description

   --------------------------
   -- Build_Format_Options --
   --------------------------

   function Build_Format_Options
     return Prettier_Ada.Documents.Format_Options_Type
   is
   begin
      return Options : Prettier_Ada.Documents.Format_Options_Type do
         Options.Width := Document_Formatter.Command_Line.Print_Width.Get;
         if Document_Formatter.Command_Line.Use_Tabs.Get then
            Options.Indentation.Kind := Prettier_Ada.Documents.Tabs;
         else
            Options.Indentation.Kind := Prettier_Ada.Documents.Spaces;
         end if;
         Options.Indentation.Width :=
           Document_Formatter.Command_Line.Indent_Width.Get;
         Options.Indentation.Offset.Spaces :=
           Document_Formatter.Command_Line.Indentation_Offset_Spaces.Get;
         Options.Indentation.Offset.Tabs :=
           Document_Formatter.Command_Line.Indentation_Offset_Tabs.Get;
         Options.End_Of_Line :=
           (case Document_Formatter.Command_Line.End_Of_Line.Get is
              when Document_Formatter.Command_Line.LF   =>
                Prettier_Ada.Documents.LF,
              when Document_Formatter.Command_Line.CR   =>
                Prettier_Ada.Documents.CR,
              when Document_Formatter.Command_Line.CRLF =>
                Prettier_Ada.Documents.CRLF,
              when Document_Formatter.Command_Line.AUTO =>
                Detect_End_Of_Line);
      end return;
   end Build_Format_Options;

   ------------------------
   -- Detect_End_Of_Line --
   ------------------------

   function Detect_End_Of_Line return Prettier_Ada.Documents.End_Of_Line_Kind
   is
   begin
      --  TODO: Implement this function
      return Prettier_Ada.Documents.LF;
   end Detect_End_Of_Line;

   use type GNAT.Strings.String_Access;
   use type GNATCOLL.VFS.Virtual_File;
   use type Prettier_Ada.Documents.Document_Type;

begin
   GNATCOLL.Traces.Parse_Config_File;

   if Document_Formatter.Command_Line.Parser.Parse then
      if Document_Formatter.Command_Line.Verbose.Get then
         Document_Formatter_Trace.Set_Active (True);
      end if;

      if Document_Formatter.Command_Line.Document.Get
           /= GNATCOLL.VFS.No_File
      then
         declare
            Document_Text : GNAT.Strings.String_Access :=
              GNATCOLL.VFS.Read_File
                (Document_Formatter.Command_Line.Document.Get);

         begin
            if Document_Text = null then
               --  TODO: Print error message to the user and exit with code
               --  /= 0.
               raise Program_Error;
            end if;

            declare
               Document : constant Prettier_Ada.Documents.Document_Type :=
                  Prettier_Ada.Documents.Json.Deserialize
                    (Document_Text.all);

            begin

               GNAT.Strings.Free (Document_Text);
               if Document
                  = Prettier_Ada.Documents.Builders.No_Document
               then
                  --  TODO: Print error message to the user and exit with
                  --  code /= 0.
                  raise Program_Error;
               end if;
               Ada.Text_IO.Unbounded_IO.Put_Line
                 (Prettier_Ada.Documents.Format
                    (Document, Build_Format_Options));
            end;
         end;
      end if;
   end if;

end Document_Formatter.Main;
