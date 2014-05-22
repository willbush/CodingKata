package com.willbush.framework;

import java.util.ArrayList;
import java.util.List;

public class Pool<T> {
    public interface PoolObjectFactor<T> {
        public T createObject();
    }

    private final List<T> freeObjects;
    private final PoolObjectFactor<T> factory;
    private final int maxSize;

    public Pool(PoolObjectFactor<T> factory, int maxSize) {
        this.factory = factory;
        this.maxSize = maxSize;
        this.freeObjects = new ArrayList<>(maxSize);
    }

    public T newOject() {
        T object = null;

        if (freeObjects.size() == 0)
            object = factory.createObject();
        else
            object = freeObjects.remove(freeObjects.size() - 1);

        return object;
    }

    public void free(T object) {
        if (freeObjects.size() < maxSize)
            freeObjects.add(object);
    }
}
