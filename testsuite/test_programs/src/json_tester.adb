--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Traces;

with Prettier_Ada.Documents;      use Prettier_Ada.Documents;
with Prettier_Ada.Documents.Json; use Prettier_Ada.Documents.Json;

--  Test program to build a Document from a JSON representation and then build
--  back the JSON representation from the Document.

procedure JSON_Tester is
begin
   GNATCOLL.Traces.Parse_Config_File;

   if Argument_Count /= 1 then
      Put_Line
        ("error: exactly one argument expected: the JSON file to parse");
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      Filename : constant String := Argument (1);
      Text     : constant String_Access :=
        GNATCOLL.VFS.Read_File (Create (+Filename));
      Doc      : Document_Type;
   begin
      Doc := Deserialize (Text.all);
      Put_Line (Serialize (Doc));
   end;
end JSON_Tester;
