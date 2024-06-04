--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.VFS;

package Document_Formatter.Command_Line is

   Parser : Argument_Parser :=
     Create_Argument_Parser
       (Help => "Prettier Document Formatter");

   package Verbose is new Parse_Flag
     (Parser   => Parser,
      Short    => "-v",
      Long     => "--verbose",
      Help     => "Print debug traces");

   function To_Virtual_File
     (File_Name : String) return GNATCOLL.VFS.Virtual_File
   is (GNATCOLL.VFS.Create (GNATCOLL.VFS."+" (File_Name)));

   package Document is new Parse_Option
     (Parser      => Parser,
      Short       => "-D",
      Long        => "--document",
      Help        => "Format from a JSON encoded document",
      Arg_Type    => GNATCOLL.VFS.Virtual_File,
      Convert     => To_Virtual_File,
      Default_Val => GNATCOLL.VFS.No_File);

   package Print_Width is new Parse_Option
     (Parser      => Parser,
      Long        => "--print-width",
      Help        => "",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 80);

   package Indent_Width is new Parse_Option
     (Parser      => Parser,
      Long        => "--indent-width",
      Help        => "",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 2);

   package Use_Tabs is new Parse_Flag
     (Parser      => Parser,
      Long        => "--use-tabs",
      Help        => "");

   type End_Of_Line_Kind is (LF, CRLF, CR, AUTO);

   package End_Of_Line is new Parse_Option
     (Parser      => Parser,
      Long        => "--end-of-line",
      Help        => "",
      Arg_Type    => End_Of_Line_Kind,
      Convert     => End_Of_Line_Kind'Value,
      Default_Val => LF);

   package Indentation_Offset is new Parse_Option
     (Parser      => Parser,
      Long        => "--indentation-offset",
      Help        => "",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 0);

end Document_Formatter.Command_Line;
