--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Vectors;
with Prettier_Ada.Documents;

package Prettier_Ada.Document_Vectors is new
   Ada.Containers.Vectors
     (Positive,
      Prettier_Ada.Documents.Document_Type,
      Prettier_Ada.Documents."=");
