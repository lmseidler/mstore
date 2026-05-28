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
      new_record("hBN", hbn), &
      new_record("ZnO", zno) &
      ]

end subroutine get_inorganic_records


subroutine nacl(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 8
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Na", "Na", "Na", "Na", "Cl", "Cl", "Cl", "Cl"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      &  5.26432932735840_wp,  5.26432932735840_wp,  0.00000000000000_wp, &
      &  5.26432932735840_wp,  0.00000000000000_wp,  5.26432932735840_wp, &
      &  0.00000000000000_wp,  5.26432932735840_wp,  5.26432932735840_wp, &
      &  5.26432932735840_wp,  5.26432932735840_wp,  5.26432932735840_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  5.26432932735840_wp, &
      &  0.00000000000000_wp,  5.26432932735840_wp,  0.00000000000000_wp, &
      &  5.26432932735840_wp,  0.00000000000000_wp,  0.00000000000000_wp], &
      & shape(xyz))
   real(wp), parameter :: lattice(3, 3) = reshape([&
      & 10.52865865471681_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp, 10.52865865471681_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, 10.52865865471681_wp], &
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
      &  4.30310579452643_wp,  4.30310579452643_wp,  2.78504968573567_wp, &
      &  2.63091888277346_wp,  2.63091888277346_wp,  0.00000000000000_wp, &
      &  5.97529270627940_wp,  5.97529270627940_wp,  0.00000000000000_wp, &
      &  1.67218691175297_wp,  6.93402467729987_wp,  2.78504968573567_wp, &
      &  6.93402467729987_wp,  1.67218691175297_wp,  2.78504968573567_wp], &
      & shape(xyz))
   real(wp), parameter :: lattice(3, 3) = reshape([&
      &  8.60621158905284_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp,  8.60621158905284_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  5.57009937147132_wp], &
      & shape(lattice))
   call new(self, sym, xyz, lattice=lattice)
end subroutine rutile


subroutine hbn(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "N", "N", "B", "B"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  0.00000000000000_wp,  2.93460660137097_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  8.80381980411291_wp, &
      &  0.00000000000000_wp,  2.70338066466883_wp,  2.93460660137097_wp, &
      &  2.34119633170286_wp,  1.35169033233441_wp,  8.80381980411291_wp], &
      & shape(xyz))
   real(wp), parameter :: lattice(3, 3) = reshape([&
      &  4.68239266340573_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      & -2.34119633170286_wp,  4.05507099700324_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp, 11.73842640548388_wp], &
      & shape(lattice))
   call new(self, sym, xyz, lattice=lattice)
end subroutine hbn


subroutine zno(self)
   type(structure_type), intent(out) :: self
   integer, parameter :: nat = 4
   character(len=*), parameter :: sym(nat) = [character(len=4)::&
      & "Zn", "Zn", "O", "O"]
   real(wp), parameter :: xyz(3, nat) = reshape([&
      &  0.00000000000000_wp,  3.54586013263111_wp,  0.00000000000000_wp, &
      &  3.07080495312500_wp,  1.77293006631555_wp,  4.91328792500000_wp, &
      &  0.00000000000000_wp,  3.54586013263111_wp,  3.75375197470000_wp, &
      &  3.07080495312500_wp,  1.77293006631555_wp,  8.66703989970000_wp], &
      & shape(xyz))
   real(wp), parameter :: lattice(3, 3) = reshape([&
      &  6.14160990625000_wp,  0.00000000000000_wp,  0.00000000000000_wp, &
      & -3.07080495312500_wp,  5.31879019894666_wp,  0.00000000000000_wp, &
      &  0.00000000000000_wp,  0.00000000000000_wp,  9.82657585000000_wp], &
      & shape(lattice))
   call new(self, sym, xyz, lattice=lattice)
end subroutine zno

end module mstore_inorganic
