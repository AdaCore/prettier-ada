--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded;

--  This packages provides a serialization/deserialization function that
--  encodes/decodes a Document_Type.
package Prettier_Ada.Documents.Json is

   --  TODO: Describe the json schema used to encode each document kind.

   function Serialize
     (Document : Document_Type)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Serializes Document

   function Deserialize (Document : String) return Document_Type;
   --  Deserializes Document

end Prettier_Ada.Documents.Json;