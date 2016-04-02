mv .git/objects/pack/pack-*.pack .
cat pack-*.pack | git unpack-objects
rm -f pack-*.pack
