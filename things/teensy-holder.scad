/*
--- BEGIN AGPLv3-preamble ---
Dactyl Marshmallow ergonomic keyboard generator
Copyright (C) 2015, 2018 Matthew Adereth and Jared Jennings

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
--- END AGPLv3-preamble ---
*/
/*
*/
frame_thickness = 4;
small=0.01;
tau=360;

/*
 * started with https://www.pjrc.com/teensy/dimensions.html.
 *
 * teensy lc. tweaked as necessary after printing
 */
tb_width = 18.1;
tb_length = 33.3;
tb_thickness = 1.7;
tb_margin = 0.8;

/* https://www.cs.jhu.edu/~carlson/download/datasheets/Micro-USB_1_01.pdf
*/
overmold_width = 11;
overmold_height = 9;

/* How much smaller the bumps are than the holes. First refined in the
 * frame glue joints. */
bump_tolerance = 0.1;
/* Hole diameter is set empirically, so that given the fn used, it
looks like the walls of the hole will be thick enough */
hole_diameter = frame_thickness - 0.5;
hole_fn = 7;

/* M3 screw */
screw_diameter = 3;

module teensy_holder() {
    difference() {
        union() {
             /* square at USB end leaving clearance for largest
              * possible connector overmold */
            translate([0, 
            tb_length/2+frame_thickness/2, 0]) {
                cube(size=[
                overmold_width+2*frame_thickness,
                frame_thickness,
                overmold_height+2*frame_thickness],
                center=true);
            }
            /* main frame */
            cube(size=[
                tb_width+2*frame_thickness,
                tb_length+2*frame_thickness,
                frame_thickness], center=true);
        }
        /* the board */
        cube(size=[
            tb_width, tb_length,
            tb_thickness],
            center=true);
        /* a little extra plastic on top and below the board, to hold
         * it in */
        cube(size=[
            tb_width - tb_margin,
            tb_length - tb_margin,
            frame_thickness+small],
            center=true);
        /* connector overmold */
        translate([0, tb_length/2, 0]) {
            cube(size=[
            overmold_width,
            2*frame_thickness+small,
            overmold_height], center=true);
        }
        /* you know, i bet we don't need the bottom half of that
         * overmold surround */
        translate([0, 0, -(frame_thickness/2 + overmold_height/2)]) {
             cube(size=[2*tb_width, 2*tb_length, overmold_height],
                  center=true);
        }
    }
}

/* left of x=0 */
module tab () {
     translate([-screw_diameter*2, 0, 0]) {
          difference () {
               union () {
                    cylinder(h=frame_thickness,
                             d=screw_diameter+frame_thickness*2,
                             center=true,
                             $fn=15);
                    translate([screw_diameter, 0, 0]) {
                         cube([screw_diameter*2,
                               screw_diameter+frame_thickness*2,
                               frame_thickness],
                              center=true);
                    }
               }
               cylinder(h=frame_thickness+small,
                        d=screw_diameter,
                        center=true,
                        $fn=15);
          }
     }
}

module teensy_holder_piece_a () {
     translate([(tb_width/2+frame_thickness+screw_diameter*2),
                0,
                (-frame_thickness/2)]) {
          union() {
               difference() {
                    intersection() {
                         translate([0, frame_thickness+tb_margin/2+small/2, 0]) {
                              cube(size=[
                                        tb_width+2*frame_thickness+small,
                                        tb_length+2*frame_thickness+small,
                                        overmold_height+2*frame_thickness+small],
                                   center=true);
                         }
                         teensy_holder() {}
                    }
                    translate([-(tb_width/2 + frame_thickness/2),
                               -(tb_length/2 + tb_margin),
                               0]) {
                         rotate(-tau/4,[1,0,0]) {
                              cylinder(h=3,
                                       d1=hole_diameter,
                                       d2=hole_diameter/2,
                                       $fn=hole_fn);
                         }
                    }
                    translate([(tb_width/2 + frame_thickness/2),
                               -(tb_length/2 + tb_margin),
                               0]) {
                         rotate(-tau/4,[1,0,0]) {
                              cylinder(h=3,
                                       d1=hole_diameter,
                                       d2=hole_diameter/2,
                                       $fn=hole_fn);
                         }
                    }
               }
               translate([-(tb_width/2+frame_thickness), 0, 0]) {
                    tab() {}
               }
          }
     }
}

module teensy_holder_piece_b () {
     translate([(tb_width/2+frame_thickness+screw_diameter*2),
                0,
                (-frame_thickness/2)]) {
          union() {
              intersection() {
                   translate([0, -(tb_length+frame_thickness)/2, 0]) {
                        cube(size=[tb_width+2*frame_thickness+small,
                                   frame_thickness+tb_margin,
                                   overmold_height+2*frame_thickness+small],
                             center=true);
                   }
                   teensy_holder() {}
              }
              translate([-(tb_width/2 + frame_thickness/2),
                         -(tb_length/2 + tb_margin),
                         0]) {
                   rotate(-tau/4,[1,0,0]) {
                        cylinder(h=3,
                                 d1=hole_diameter-(bump_tolerance*2),
                                 d2=hole_diameter/2-(bump_tolerance*2),
                                 $fn=hole_fn);
                   }
              }
              translate([(tb_width/2 + frame_thickness/2),
                         -(tb_length/2 + tb_margin),
                         0]) {
                   rotate(-tau/4,[1,0,0]) {
                        cylinder(h=3,
                                 d1=hole_diameter-(bump_tolerance*2),
                                 d2=hole_diameter/2-(bump_tolerance*2),
                                 $fn=hole_fn);
                   }
              }
          }
     }
}

