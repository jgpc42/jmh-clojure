package io.github.jgpc42.jmh.sample;

import org.openjdk.jmh.annotations.*;

@State(Scope.Thread)
@AuxCounters(AuxCounters.Type.OPERATIONS)
public class Counters {
    public int metric;
}
