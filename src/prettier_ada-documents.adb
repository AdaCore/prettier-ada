--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

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

   -----------
   -- Image --
   -----------

   function Image (Symbol : Symbol_Type) return String is
   begin
      return Trim (Symbol'Image, Left);
   end Image;

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

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Document_Type) is
   begin
      if Self.Bare_Document /= null then
         Self.Bare_Document.Ref_Count := @ + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Document_Type) is
      use Prettier_Ada.Documents.Implementation;

      procedure Free is new Ada.Unchecked_Deallocation
        (Bare_Document_Record, Bare_Document_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Command_Type, Command_Access);
   begin
      if Self.Bare_Document /= null then
         if Self.Bare_Document.Ref_Count = 1 then
            if Self.Bare_Document.Kind = Document_Command then
               Free (Self.Bare_Document.Command);
            end if;
            Free (Self.Bare_Document);
         else
            Self.Bare_Document.Ref_Count := @ - 1;
         end if;
      end if;
   end Finalize;

end Prettier_Ada.Documents;
