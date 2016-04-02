mv .git/object/pack/pack-*.pack .
cat pack-*.pack | git-unpack-objects
rm pack-*.pack
