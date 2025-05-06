package in.juspay.mobility.sdk.core;


import static java.lang.Class.forName;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONArray;
import org.json.JSONObject;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

@SuppressWarnings("FieldCanBeLocal")
public class InflateJSON extends InflateView {


    private final HashMap<String, HashMap<String, Object>> localState;
    @NonNull
    private final HashMap<Cmd, Constructor<?>> constructorCache = new HashMap<>();

    private final String RUNIN_UI_JSON = "rj";

    private final String JSON_ARRAY = "jsa";

    private final String JSON_OBJECT = "jso";
    private final AtomicInteger idCounter = new AtomicInteger(0);

    private final String VALUE = "v";

    private final String VOID = "vo";

    private final String COMMAND = "c";

    private final String CATCH = "ct";
    private final String TYPE = "t";

    private final String TO = "to";

    private final String GLOBAL = "g";

    private final String LOCAL = "lcl";

    private final String STATE = "st";

    private final String INVOKE_ON = "io";

    private final String RETURN_TO = "rt";

    private final String METHOD_NAME = "mn";

    private final String ARGS = "a";

    private final String NEW = "n";

    private final String EXPLICIT_TYPE = "et";

    private final String CONDITION = "cnd";

    private final String CONDITIONS = "cnds";

    private final String RETURN_TYPE = "rty";

    private final String BODY = "bd";

    private final String IF = "if";

    private final String WHILE = "w";
    private final String VALUE_SET = "vs";

    private final String VALUE_GET = "vg";

    private final String FUNCTION_STACK_NAME = "fnstk";

    private final String STATIC = "stc";

    InflateJSON(@NonNull DynamicUI dynamicUI) {
        super(dynamicUI);
        this.localState = new HashMap<>();
    }

    public Object callFunction(String fnName, Object instance, Object[] args) {
        return callFunction(fnName, instance, args, null);
    }

    @Nullable
    public Object callFunction(String fnName, Object instance, Object[] args, LinkedList<String> runningFunctions) {
        Object result;
        HashMap<String, Object> map = new HashMap<>();
        String newFunctionName = fnName + idCounter.getAndIncrement();
        if (runningFunctions == null) {
            runningFunctions = new LinkedList<>();
        }
        map.put(FUNCTION_STACK_NAME, runningFunctions);
        localState.put(newFunctionName, map);
        runningFunctions.add(newFunctionName);
        map.put(ARGS, args);
        JSONArray commands = getDUI().getFunction(fnName);
        result = runJSON(instance, commands, false, runningFunctions);
        runningFunctions.removeLast();
        localState.remove(newFunctionName);

        return result;
    }

    @Nullable
    private Object getLocalStateValue(String key, LinkedList<String> runningFunctions) throws Exception {
        Iterator<String> iterator = runningFunctions.descendingIterator();
        while (iterator.hasNext()) {
            String fnName = iterator.next();
            if (!localState.containsKey(fnName)) {
                throw new Exception("local state not found for function " + fnName);
            }
            if (Objects.requireNonNull(localState.get(fnName)).containsKey(key)) {
                return Objects.requireNonNull(localState.get(fnName)).get(key);
            }
        }
        return null;
    }

    private void setLocalStateValue(String key, Object value, LinkedList<String> runningFunctions) {
        String fnName = runningFunctions.getLast();
        Objects.requireNonNull(localState.get(fnName)).put(key, value);
    }

    @Nullable
    private Object getStateValue(String key) {
        return state.get(key);
    }

    private void saveOutput(Object output, JSONObject returnTo, LinkedList<String> runningFunctions) throws Exception {
        if (returnTo == null)
            return;
        String to = returnTo.getString(TO);
        String key = returnTo.getString(VALUE);
        if (to.equals(GLOBAL)) {
            getDUI().setGlobalState(key, output);
        } else if (to.equals(LOCAL)) {
            setLocalStateValue(key, output, runningFunctions);
        } else {
            state.put(key, output);
        }
    }


    @Nullable
    public Object getValue(JSONObject invokeOn, Object instance, LinkedList<String> runningFunctions) throws Exception {
        String type = invokeOn.getString(TYPE);
        if (type.equals(JSON_ARRAY)) {
            return invokeOn.getJSONArray(VALUE);
        } else if (type.equals(JSON_OBJECT)) {
            return invokeOn.getJSONObject(VALUE);
        }
        String val = invokeOn.getString(VALUE);
        return getValueNew(type, val, instance, runningFunctions);
    }

    @SuppressWarnings("unchecked")
    private <Any> Any getClassTypeFromObject(Object toConvert, @NonNull String subtype) throws ClassNotFoundException {
        if (!subtype.equals("")) {
            Any primitive = (Any) createPrimitiveClass(subtype);
            return primitive == null ? (Any) Class.forName(subtype) : primitive;
        }
        return (Any) toConvert.getClass();
    }

    @Nullable
    public Object runProps(JSONObject obj, String methodName, Object proxy) throws Exception {
        JSONObject methodObj = obj.getJSONObject(methodName);
        JSONObject props = methodObj.getJSONObject("props");
        Iterator<String> keys = props.keys();
        while (keys.hasNext()) {
            String key = keys.next();
            parseKeys(key, props, proxy, false);
        }
        Object result = getState().get(RETURN_TO);
        String returnType = methodObj.getString(RETURN_TYPE);
        if (returnType.equals(VOID)) {
            return null;
        }
        Class<?> returnClass = forName(returnType);
        if (returnClass.isInstance(result)) {
            return result;
        }
        getDUI().getLogger().e("WARNING", "return type mismatch for method " + methodName + " expected " + returnType + " got " + (result != null ? result.getClass().getName() : "result isnull"));
        JSONArray commands = methodObj.getJSONArray(CATCH);
        result = runJSON(proxy, commands, false, null);
        if (returnClass.isInstance(result)) {
            return result;
        }
        getDUI().getLogger().e("WARNING", "return type mismatch for method on default" + methodName + " expected " + returnType + " got " + (result == null ? "null" : result.getClass().getName()));
        return null;

    }

    @SuppressWarnings("unchecked")
    @Nullable
    public <Any> Any getValueNew(String type, String value, Object instance, LinkedList<String> runningFunctions) throws Exception {
        switch (type) {
            case STATIC:
                return null;
            case "this":
                return (Any) instance;
            case "infl":
                return (Any) this;
            case "ctx":
                Context ctx = dynamicUI.getActivity();
                if (ctx == null) {
                    ctx = dynamicUI.getAppContext();
                }
                return (Any) ctx;
            case GLOBAL:
                return (Any) getDUI().getGlobalState(value);
            case LOCAL:
                return (Any) getLocalStateValue(value, runningFunctions);
            case STATE:
                return (Any) getStateValue(value);
            default:
                return getValueNew(type, value);
        }
    }


    @Nullable
    protected Method findMethodInClassWithArgs(Class<?> c, String functionName, Class<?>[] args) throws Exception {
        return findMethodWithCmd(new Cmd(c, functionName, args));
    }

    private Field getField(Class<?> c, String fieldName) throws Exception {
        return c.getDeclaredField(fieldName);
    }

    @Nullable
    public Object runCommandJSON(JSONObject command, Object instance, boolean useAppContext, LinkedList<String> runningFunctions) throws Exception {


        String type = command.optString(TYPE, COMMAND);
        JSONObject invokeOn = command.optJSONObject(INVOKE_ON);
        String classMethodDetails = command.optString(METHOD_NAME, "");
        Object toRunOn = null;
        Class<?> className = null;
        Arguments arguments = null;
        if (invokeOn != null) {
            toRunOn = getValue(invokeOn, instance, runningFunctions);
            className = getClassNameJSON(invokeOn, toRunOn);
            arguments = new Arguments(command.optJSONArray(ARGS), instance, runningFunctions);
        }


        switch (type) {
            case VALUE_SET:
                Field fieldToChange = getField(Objects.requireNonNull(className), classMethodDetails);
                fieldToChange.set(toRunOn, Objects.requireNonNull(arguments).args[0]);
                return null;
            case VALUE_GET:
                Field fieldToChange1 = getField(Objects.requireNonNull(className), classMethodDetails);
                return fieldToChange1.get(toRunOn);
            case WHILE:
                JSONArray whileCondition = command.getJSONArray(CONDITION);
                JSONArray body = command.getJSONArray(BODY);
                while (true) {
                    Object output = runJSON(instance, whileCondition, useAppContext, runningFunctions);
                    if (output instanceof Boolean && ((Boolean) output).equals(true)) {
                        runJSON(instance, body, useAppContext, runningFunctions);
                    } else {
                        break;
                    }
                }
                break;
            case IF:
                JSONArray conditions = command.getJSONArray(CONDITIONS);
                for (int i = 0; i < conditions.length(); i++) {
                    JSONObject conditionObject = conditions.getJSONObject(i);
                    JSONArray condition = conditionObject.getJSONArray(CONDITION);
                    Object output = runJSON(instance, condition, useAppContext, runningFunctions);
                    if (output instanceof Boolean && ((Boolean) output).equals(true)) {
                        JSONArray newCommands = conditionObject.getJSONArray(BODY);

                        return runJSON(instance, newCommands, useAppContext, runningFunctions);
                    }

                }
                break;
            default:

                if (classMethodDetails.equals(NEW)) {
                    return createNewInstance(Objects.requireNonNull(className), Objects.requireNonNull(arguments).args, arguments.classTypes);
                }

                Method toInvoke = findMethodInClassWithArgs(className, classMethodDetails, Objects.requireNonNull(arguments).classTypes);
                if (toInvoke == null) {
                    throw new Exception("Method not found");
                }

                return invokeFunction(toInvoke, toRunOn, arguments.args);
        }


        return null;
    }

    @Nullable
    public Object parseAndRunPipeJSON(Object instance, JSONArray toParse, boolean useAppContext, LinkedList<String> runningFunctions) throws Exception {
        Object result = null;
        for (int i = 0; i < toParse.length(); i++) {
            JSONObject command = toParse.getJSONObject(i);
            result = runCommandJSON(command, instance, useAppContext, runningFunctions);
            if (command.has(RETURN_TO)) {
                saveOutput(result, command.getJSONObject(RETURN_TO), runningFunctions);
            }
        }
        return result;

    }

    @Nullable
    public Object runJSON(Object instance, JSONArray commands, boolean useApplContext, LinkedList<String> runningFunctions) {
        Object result = null;
        try {
            for (int i = 0; i < commands.length(); i++) {
                JSONArray command = commands.getJSONArray(i);
                result = parseAndRunPipeJSON(instance, command, useApplContext, runningFunctions);
            }
        } catch (Exception e) {
            dynamicUI.getLogger().e("WARNING", "Error in parsing new infl " + e.getMessage());
        }
        return result;

    }

    @Override
    public void parseKeys(String key, JSONObject properties, Object instance, boolean useApplContext) {
        try {
            if (key.equals(RUNIN_UI_JSON)) {
                JSONArray commands = properties.getJSONArray(RUNIN_UI_JSON);
                runJSON(instance, commands, useApplContext, null);
                return;
            }

        } catch (Exception e) {
            dynamicUI.getLogger().e("WARNING", "Error in parsing new infl " + e.getMessage());
        }

        super.parseKeys(key, properties, instance, useApplContext);
    }

    @Nullable
    public Class<?> getClassNameJSON(JSONObject toInvoke, Object toRunOn) throws Exception {
        String type = toInvoke.getString(TYPE);
        String className = toInvoke.optString(EXPLICIT_TYPE);
        if (type.equals(STATIC)) {
            className = toInvoke.getString(VALUE);
        }
        if (!className.equals("")) {
            return getClassName(className);
        }
        if (toRunOn == null) {
            throw new Exception("toRunOn is null");
        }
        return toRunOn.getClass();
    }

    @Nullable
    private Object invokeFunction(Method toInvoke, Object toRunOn, Object[] argsDetails) throws Exception {
        if (argsDetails == null)
            return toInvoke.invoke(toRunOn);
        return toInvoke.invoke(toRunOn, argsDetails);
    }


    private class Arguments {
        final private Object[] args;
        private Class<?>[] classTypes;

        public Arguments(JSONArray args, Object instance, LinkedList<String> runningFuncations) throws Exception {
            if (args == null) {
                args = new JSONArray();
                classTypes = new Class[0];
            }
            this.args = new Object[args.length()];
            this.classTypes = new Class[args.length()];
            for (int i = 0; i < args.length(); i++) {
                JSONObject arg = args.getJSONObject(i);
                this.args[i] = getValue(arg, instance, runningFuncations);
                this.classTypes[i] = getClassTypeFromObject(this.args[i], arg.optString(EXPLICIT_TYPE));
            }
        }
    }

    private Object createNewInstance(Class<?> clazz, Object[] args, Class<?>[] argClass) throws Exception {
        if (args == null || args.length == 0) {
            return clazz.newInstance();
        }
        Cmd cmd = new Cmd(clazz, "new", argClass);
        if (constructorCache.containsKey(cmd)) {
            return Objects.requireNonNull(constructorCache.get(cmd)).newInstance(args);
        }
        Object result = null;
        Constructor<?> constructor = null;
        try {
            constructor = clazz.getConstructor(argClass);
            result = constructor.newInstance(args);
        } catch (NoSuchMethodException e) {
            Constructor<?>[] constructors = clazz.getConstructors();
            if (argClass == null) {
                argClass = new Class[args.length];
                for (int i = 0; i < args.length; i++) {
                    if (args[i] != null) {
                        argClass[i] = args[i].getClass();
                    }
                }
            }
            for (Constructor<?> cnstr : constructors) {
                if (cnstr.getParameterTypes().length == args.length
                        && matchTypes(cnstr.getParameterTypes(), argClass)) {
                    constructor = cnstr;
                    result = constructor.newInstance(args);
                    break;
                }
            }

        }
        constructorCache.put(cmd, constructor);
        return result;
    }
}
