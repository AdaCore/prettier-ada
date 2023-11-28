--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Prettier_Ada.Documents.Implementation;

package body Prettier_Ada.Documents is

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Document_Type) return Boolean
     renames Prettier_Ada.Documents.Implementation."=";

   ------------
   -- Format --
   ------------

   function Format
     (Document : Document_Type;
      Options  : Format_Options_Type := Default_Format_Options)
      return Ada.Strings.Unbounded.Unbounded_String
     renames Prettier_Ada.Documents.Implementation.Format;

   ----------
   -- Hash --
   ----------

   function Hash (Document : Document_Type) return Ada.Containers.Hash_Type
     renames Prettier_Ada.Documents.Implementation.Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Symbol : Symbol_Type) return Ada.Containers.Hash_Type
     renames Prettier_Ada.Documents.Implementation.Hash;

   ----------------
   -- New_Symbol --
   ----------------

   function New_Symbol return Symbol_Type
     renames Prettier_Ada.Documents.Implementation.New_Symbol;

   ----------------------
   -- To_Document_Type --
   ----------------------

   function To_Document_Type (Text : Wide_Wide_String) return Document_Type
     renames Prettier_Ada.Documents.Implementation.To_Document_Type;

end Prettier_Ada.Documents;
