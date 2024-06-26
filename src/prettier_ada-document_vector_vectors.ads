--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Vectors;
with Prettier_Ada.Document_Vectors;

package Prettier_Ada.Document_Vector_Vectors is new
   Ada.Containers.Vectors
     (Positive,
      Prettier_Ada.Document_Vectors.Vector,
      Prettier_Ada.Document_Vectors."=");
