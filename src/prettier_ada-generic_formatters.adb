--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Prettier_Ada.Generic_Formatters is

   -----------
   -- Print --
   -----------

   function Print
     (Node    : Node_Type;
      Options : Prettier_Ada.Documents.Format_Options_Type)
      return Ada.Strings.Unbounded.Unbounded_String
   is (Prettier_Ada.Documents.Format (Print_IR (Node), Options));

end Prettier_Ada.Generic_Formatters;
