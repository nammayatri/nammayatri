package in.juspay.mobility.common;

import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteException;
import android.database.sqlite.SQLiteStatement;
import android.util.Base64;

import java.io.File;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SQLiteAndroidDatabase {
    private static final Pattern FIRST_WORD = Pattern.compile("^\\s*(\\S+)", Pattern.CASE_INSENSITIVE);

    private File dbFile;
    private int openFlags;
    private SQLiteDatabase mydb;

    public void open(File dbfile, int openFlags) throws Exception {
        this.dbFile = dbfile;
        this.openFlags = openFlags;
        this.mydb = SQLiteDatabase.openDatabase(dbfile.getAbsolutePath(), null, openFlags);
    }

    public void closeDatabaseNow() {
        if (mydb != null) {
            mydb.close();
            mydb = null;
        }
    }

    public void executeSqlBatch(String[] queryArr) {
        if (mydb == null) {
            throw new IllegalStateException("database has been closed");
        }

        String query = "";
        int len = queryArr.length;

        for (int i = 0; i < len; i++) {
            try {
                query = queryArr[i];
                QueryType queryType = getQueryType(query);

                if (queryType == QueryType.update || queryType == QueryType.delete) {
                    SQLiteStatement myStatement = mydb.compileStatement(query);
                    myStatement.executeUpdateDelete();
                } else if (queryType == QueryType.insert) {
                    SQLiteStatement myStatement = mydb.compileStatement(query);
                    myStatement.executeInsert();
                } else if (queryType == QueryType.begin) {
                    mydb.beginTransaction();
                } else if (queryType == QueryType.commit) {
                    mydb.setTransactionSuccessful();
                    mydb.endTransaction();
                } else if (queryType == QueryType.rollback) {
                    mydb.endTransaction();
                } else {
                    mydb.rawQuery(query, null);
                }
            } catch (Exception ex) {
                throw new RuntimeException("SQLiteAndroidDatabase.executeSqlBatch() failed", ex);
            }
        }
    }

    private static QueryType getQueryType(String query) {
        Matcher matcher = FIRST_WORD.matcher(query);
        if (matcher.find()) {
            try {
                return QueryType.valueOf(matcher.group(1).toLowerCase(Locale.US));
            } catch (IllegalArgumentException ignore) {
                // unknown verb
            }
        }
        return QueryType.other;
    }

    private enum QueryType {
        update,
        insert,
        delete,
        select,
        begin,
        commit,
        rollback,
        other
    }
}