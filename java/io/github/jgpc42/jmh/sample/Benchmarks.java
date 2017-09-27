package io.github.jgpc42.jmh.sample;

import org.openjdk.jmh.annotations.*;

@State(Scope.Benchmark)
public class Benchmarks {
    @Param({"0"})
    public int amount;

    @Benchmark
    @BenchmarkMode(Mode.SingleShotTime)
    public void spin () throws InterruptedException {
        if (amount == 0)
            throw new AssertionError("parameter not set");
        Thread.sleep(amount);
    }
}
