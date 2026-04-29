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
      new_record("NaCl", nacl), &
      new_record("rutile", rutile), &
      new_record("hBN", hbn) &
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


subroutine rutile(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 6
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Ti", "Ti", "O", "O", "O", "O"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      &  2.27710552211709_wp,  2.27710552211709_wp,  1.47378482463202_wp, &
      &  1.39222231622239_wp,  1.39222231622239_wp,  0.00000000000000_wp, &
      &  3.16198872801179_wp,  3.16198872801179_wp,  0.00000000000000_wp, &
      &  0.88488320589470_wp,  3.66932783833947_wp,  1.47378482463202_wp, &
      &  3.66932783833947_wp,  0.88488320589470_wp,  1.47378482463202_wp], &
      & shape(xyz))
   real(wp), parameter :: lattice(3, 3) = reshape([&
      &  4.55421104423417_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp,  4.55421104423417_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.94756964926403_wp], &
      & shape(lattice))
   call new(self, sym, xyz, lattice=lattice)
end subroutine rutile


subroutine hbn(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "N", "N", "B", "B"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  1.55292693610349_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  4.65878080831047_wp, &
      &  0.00000000000000_wp,  1.43056743985525_wp,  1.55292693610349_wp, &
      &  1.23890774474151_wp,  0.71528371992762_wp,  4.65878080831047_wp], &
      & shape(xyz))
   real(wp), parameter :: lattice(3, 3) = reshape([&
      &  2.47781548948302_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      & -1.23890774474151_wp,  2.14585115978287_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  6.21170774441396_wp], &
      & shape(lattice))
   call new(self, sym, xyz, lattice=lattice)
end subroutine hbn

end module mstore_inorganic
