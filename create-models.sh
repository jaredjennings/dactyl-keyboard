set -ex
for size in 4x5 4x6 5x6 6x6; do
    [ $size = 4x5 ] || patch -p1 < ${size}.patch
    lein run src/dactyl_keyboard/dactyl.clj
    for piece in "" "case-"; do
        for side in right left; do
            b="things/${piece}${side}"
            s="${b}-${size}"
            cp "${b}.scad" "${s}.scad"
            openscad -o "${s}.stl" "${s}.scad" \
                     >/dev/null 2>&1 &
        done
    done
    cp things/right-plate.scad \
       things/right-${size}-plate.scad
    openscad -o things/right-${size}-plate.dxf \
             things/right-${size}-plate.scad \
             >/dev/null 2>&1 &
    wait
    git checkout src/dactyl_keyboard/dactyl.clj
done

# git add things/*-4x5.stl
# git add things/right-4x5-plate.dxf
# git commit -m "Add CAD files"
