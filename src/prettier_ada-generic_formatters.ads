--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded;

with Prettier_Ada.Documents;

--  This package provides a generic printer.
--  Users need to instantiate with with the type that represents the source
--  code to be formatted
--  (usually an AST type, such as Libadalang.Analysis.Ada_Node) and a Print_IR
--  function that converts it into a Document_Type. This function should use
--  the Prettier_Ada.Documents.Builders package to build a Document_Type.

generic
   type Node_Type is private;
   with function Print_IR
     (Node : Node_Type)
      return Prettier_Ada.Documents.Document_Type;
package Prettier_Ada.Generic_Formatters is
   function Print
     (Node    : Node_Type;
      Options : Prettier_Ada.Documents.Format_Options_Type)
      return Ada.Strings.Unbounded.Unbounded_String;
end Prettier_Ada.Generic_Formatters;
