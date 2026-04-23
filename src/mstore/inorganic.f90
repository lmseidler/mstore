! This file is part of mstore.
! SPDX-Identifier: Apache-2.0
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

module mstore_inorganic
   use mctc_env_accuracy, only : wp
   use mctc_io_structure, only : structure_type, new
   use mstore_data_record, only : record_type, new_record
   implicit none
   private

   public :: get_inorganic_records

contains

subroutine get_inorganic_records(records)
   type(record_type), allocatable, intent(out) :: records(:)

   records = [ &
      new_record("NaCl", nacl) &
      ]

end subroutine get_inorganic_records


subroutine nacl(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Na", "Na", "Na", "Na", "Cl", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      &  2.78576311017471_wp,  2.78576311017471_wp,  0.00000000000000_wp, &
      &  2.78576311017471_wp,  0.00000000000000_wp,  2.78576311017471_wp, &
      &  0.00000000000000_wp,  2.78576311017471_wp,  2.78576311017471_wp, &
      &  2.78576311017471_wp,  2.78576311017471_wp,  2.78576311017471_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.78576311017471_wp, &
      &  0.00000000000000_wp,  2.78576311017471_wp,  0.00000000000000_wp, &
      &  2.78576311017471_wp,  0.00000000000000_wp,  0.00000000000000_wp], &
      & shape(xyz))
   real(wp), parameter :: lattice(3, 3) = reshape([&
      &  5.57152622034942_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp,  5.57152622034942_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  5.57152622034942_wp], &
      & shape(lattice))
   call new(self, sym, xyz, lattice=lattice)
end subroutine nacl

end module mstore_inorganic
