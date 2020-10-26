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

tau = 360; // openscad works in degrees not radians

// placement
bet = tau / 48;
tenting_angle = tau / 8;
thu_alp = tau / 30;
thu_bet = tau / 8;
// switch_hole
keyswitch_height = 14.0;
keyswitch_width = 14.0;
cherry_bezel_width = 2.0;
mount_width = keyswitch_width + 2*cherry_bezel_width;
mount_height = keyswitch_height + 2*cherry_bezel_width;
sa_profile_key_height = 12.7;
these_blank_keycaps_i_got_from_ebay_height = 11.5;
key_height = these_blank_keycaps_i_got_from_ebay_height;
plate_thickness = 4;
web_thickness = 3.5;
// placement
cap_top_height = plate_thickness + key_height;
// things added to mount_width and mount_height here and below are
// kluges. to derive them, start with 0.0, subtract if there is
// distance between the closest switch holes, add if they overlap, and
// hope the keys with non-minimal distance between them don't end up
// too far apart to reach with your fingers.
column_radius = cap_top_height + ( ((mount_width + 1.8) / 2) / sin(bet/2) );
thu_row_radius = cap_top_height + ( ((mount_height + 1/2) / 2) / sin(thu_alp/2) );
thu_column_radius = cap_top_height + ( ((mount_width + 1) / 2) / sin(thu_bet/2) );
// connectors
post_size = 0.1;

// this is a function not an array because occasionally we may deal in
// half-columns, can't remember where
function ColumnOffset(column) = ((column >= 2) && (column < 3) ? [-0.5, 5.82, -5.0] : (column >= 5) ? [0, -12.8, 5.64] : (column >= 4) ? [0, -8.8, 5.64] : [0,0,0]);

module KeyPlace(col, row) {
     row_alphas = [ tau/16, tau/24, tau/18, tau/13, tau/12, tau/12 ];
     // must be >= 1.0. related somehow to the lengths of your fingers.
     column_radius_factors = [ 1.05, 1.05, 1.12, 1.03, 1.0, 1.0, 1.0 ];
     // this is to bring the number row closer without rotating its
     // keytops to such a rakish angle
     row_compensation = [ [0,-3,2], [0,0,0], [0,0,0], [0,0,0], [0,2,0], [0,2,0] ];
     alp = row_alphas[max(floor(row),0)];
     row_radius = (cap_top_height + ( ((mount_height + -1.0) / 2) / sin(alp/2) )) * column_radius_factors[max(floor(col),0)];
     column_angle = bet * (2 - col);
     column_splay = [0, 0, 4, 6, 11, 11];
     column_splay_radius = 30; // this has an interplay with the
                               // measure added to mount_width above
                               // in column_radius
     translate([0, 0, 45])
	  rotate(a=tenting_angle, v=[0,1,0])
          translate(ColumnOffset(col))
          translate([0, 0, column_radius])
          rotate(a=column_angle, v=[0, 1, 0])
          translate([0, 0, -column_radius])
          translate([0, -column_splay_radius, 0])
          rotate(a=-column_splay[max(floor(col),0)], v=[0,0,1])
          translate([0, column_splay_radius, 0])
          translate(row_compensation[max(floor(row),0)] *
                    pow(column_radius_factors[max(floor(col),0)],2))
          translate([0, 0, row_radius])
          rotate(a=alp*(2-row), v=[1,0,0])
          translate([0, 0, -row_radius])
          children();
}

module ThumbPlace(col, row) {
     thumb_column_splay = [6, 15, 20, 15, 15];
     thumb_column_splay_radius = 12; // this has an interplay with the
                               // measure added to mount_width above
                               // in column_radius
     thumb_column_x_fudge = [0, 0, 1, 1, 1];
     thumb_column_y_fudge = [0, -4, -14, -6, -6];
     thumb_column_z_fudge = [0, 0, 5, 5, 5];
     translate([-6, -49, 51])
          rotate(a=tau/10, v=[0,0,1])
          rotate(a=-tau/8, v=[0,1,0])
          translate([mount_width, 0, 0])
          translate([0, 0, thu_column_radius])
          rotate(a=col*thu_bet, v=[0,1,0])
          translate([0, 0, -thu_column_radius])
          translate([0, -thumb_column_splay_radius, 0])
          translate([thumb_column_x_fudge[max(floor(col),0)],
                    thumb_column_y_fudge[max(floor(col),0)],
                    thumb_column_z_fudge[max(floor(col),0)]])
          rotate(a=thumb_column_splay[max(floor(col),0)], v=[0,0,1])
          translate([0, thumb_column_splay_radius, 0])
          translate([0, 0, thu_row_radius])
          rotate(a=row*thu_alp, v=[1,0,0])
          translate([0, 0, -thu_row_radius])
          children();
}


module WebPost() {
     translate ([0, 0, (plate_thickness - (web_thickness/2))]) {
          cube ([post_size, post_size, web_thickness], center=true);
     }
}

/* the multiplier here makes the post tall enough that when you take
   the difference of something that sticks up off of the web with
   structures made out of hulls of this post, no detritus will be left
   over because it extended above these AboveWebPosts. Perhaps it
   should not be hitched to web_thickness. Oh well. */
module AboveWebPost() {
     translate ([0, 0, (plate_thickness + (web_thickness*5/2))]) {
          cube ([post_size, post_size, web_thickness*5], center=true);
     }
}

module WebLogHBW() {
     translate ([-web_thickness/2, 0, (plate_thickness - web_thickness)])
          cube ([web_thickness, post_size, post_size], center=true);
}
module WebLogHBE() {
     translate ([web_thickness/2, 0, (plate_thickness - web_thickness)])
          cube ([web_thickness, post_size, post_size], center=true);
}
module WebLogHTW() {
     translate ([-web_thickness/2, 0, plate_thickness])
          cube ([web_thickness, post_size, post_size], center=true);
}
module WebLogHTE() {
     translate ([web_thickness/2, 0, plate_thickness])
          cube ([web_thickness, post_size, post_size], center=true);
}

module WebLogVBN() {
     translate ([0, web_thickness/2, (plate_thickness - web_thickness)])
          cube ([post_size, web_thickness, post_size], center=true);
}
module WebLogVBS() {
     translate ([0, -web_thickness/2, (plate_thickness - web_thickness)])
          cube ([post_size, web_thickness, post_size], center=true);
}
module WebLogVTN() {
     translate ([0, web_thickness/2, plate_thickness])
          cube ([post_size, web_thickness, post_size], center=true);
}
module WebLogVTS() {
     translate ([0, -web_thickness/2, plate_thickness])
          cube ([post_size, web_thickness, post_size], center=true);
}


module Marker(x) {
     children();
}
