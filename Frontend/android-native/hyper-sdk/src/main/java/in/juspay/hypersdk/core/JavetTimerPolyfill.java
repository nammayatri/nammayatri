//import com.caoccao.javet.interop.V8Runtime;
//import com.caoccao.javet.values.reference.V8Function;
//
//import java.util.*;
//import java.util.concurrent.*;
//import java.util.concurrent.atomic.AtomicInteger;
//
//public class JavetTimerPolyfill {
//    private final ScheduledExecutorService executorService = Executors.newScheduledThreadPool(1);
//    private final Map<Integer, ScheduledFuture<?>> scheduledTasks = new ConcurrentHashMap<>();
//    private final V8Runtime v8Runtime;
//    private final AtomicInteger timerIdCounter = new AtomicInteger(1);
//
//    public JavetTimerPolyfill(V8Runtime v8Runtime) {
//        this.v8Runtime = v8Runtime;
//    }
//
//    public void register() throws Exception {
//        v8Runtime.set("setTimeout", (IV8Executable) (receiver, parameters) -> {
//            V8Function fn = (V8Function) parameters[0];
//            int delay = parameters.length > 1 ? parameters[1].toInteger() : 0;
//
//            int timerId = timerIdCounter.getAndIncrement();
//            ScheduledFuture<?> future = executorService.schedule(() -> {
//                try {
//                    fn.callVoid(null);
//                } catch (Exception e) {
//                    e.printStackTrace();
//                }
//            }, delay, TimeUnit.MILLISECONDS);
//
//            scheduledTasks.put(timerId, future);
//            return v8Runtime.createV8ValueInteger(timerId);
//        });
//
//        v8Runtime.set("clearTimeout", (IV8Executable) (receiver, parameters) -> {
//            int id = parameters[0].toInteger();
//            ScheduledFuture<?> future = scheduledTasks.remove(id);
//            if (future != null) {
//                future.cancel(false);
//            }
//            return null;
//        });
//
//        v8Runtime.set("setInterval", (IV8Executable) (receiver, parameters) -> {
//            V8Function fn = (V8Function) parameters[0];
//            int interval = parameters.length > 1 ? parameters[1].toInteger() : 0;
//
//            int timerId = timerIdCounter.getAndIncrement();
//            ScheduledFuture<?> future = executorService.scheduleAtFixedRate(() -> {
//                try {
//                    fn.callVoid(null);
//                } catch (Exception e) {
//                    e.printStackTrace();
//                }
//            }, interval, interval, TimeUnit.MILLISECONDS);
//
//            scheduledTasks.put(timerId, future);
//            return v8Runtime.createV8ValueInteger(timerId);
//        });
//
//        v8Runtime.set("clearInterval", (IV8Executable) (receiver, parameters) -> {
//            int id = parameters[0].toInteger();
//            ScheduledFuture<?> future = scheduledTasks.remove(id);
//            if (future != null) {
//                future.cancel(false);
//            }
//            return null;
//        });
//    }
//
//    public void shutdown() {
//        executorService.shutdownNow();
//        scheduledTasks.clear();
//    }
//}
