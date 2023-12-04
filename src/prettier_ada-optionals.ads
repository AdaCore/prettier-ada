--
--  Copyright (C) 2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This packages provides a generic package that creates an optional type,
--  i.e., a type which can be set or not.

package Prettier_Ada.Optionals is

   generic
      type T is private;
   package Generic_Optional_Types is
      type Generic_Optional_Type (Is_Set : Boolean := False) is record
         case Is_Set is
            when False =>
               null;
            when True =>
               Value : T;
         end case;
      end record;
   end Generic_Optional_Types;

end Prettier_Ada.Optionals;
