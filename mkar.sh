pushd content

for class in *; do
    if [ -d $class ]; then
        pushd $class
        zip archive-cours-$class.zip */*.pdf
        popd
    fi
done

popd
