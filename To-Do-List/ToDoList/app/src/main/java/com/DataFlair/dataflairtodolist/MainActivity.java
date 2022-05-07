// this is the main activity of our to do list program. At the beginning
// we can see that all of the libraries are imported automatically when we are using our code.
package com.DataFlair.dataflairtodolist;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;

import android.os.Bundle;

import android.content.ContentValues;
import android.content.DialogInterface;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;

import com.DataFlair.dataflairtodolist.task_database.TaskContract;
import com.DataFlair.dataflairtodolist.task_database.TaskDatabaseHelper;

import java.util.ArrayList;

// Here we created a class and this is located in the MainActivity.java
public class MainActivity extends AppCompatActivity {
    private static final String TAG = "MainActivity";
    private TaskDatabaseHelper taskHelper;
    private ListView TaskList;
    private ArrayAdapter<String> arrAdapter;
    // We created a protected method to create the task list in our app
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        taskHelper = new TaskDatabaseHelper(this);
        TaskList = (ListView) findViewById(R.id.list_todo);

        updateUI();
    }
    // we are returning a boolean. This identifies the type of value expected to return after the method performs the specified tasks.
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.main_menu, menu);
        return super.onCreateOptionsMenu(menu);
    }
    // In this method we are created the messages using alert dialogs to
    // prompt the user to create the task they want to perform. They are able to see the alert dialogs with the
    // messages in green letters. Whenever they put a task in goes to a database found in task_database
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.action_add_task:
                final EditText taskEdit = new EditText(this);
                AlertDialog dialog = new AlertDialog.Builder(this)
                        .setTitle("Add a new task").setMessage("What would you want to do next?").setView(taskEdit)
                        .setPositiveButton("Add", new DialogInterface.OnClickListener() {
                            @Override
                            public void onClick(DialogInterface dialog, int which) {
                                String task = String.valueOf(taskEdit.getText());
                                SQLiteDatabase db = taskHelper.getWritableDatabase();
                                ContentValues values = new ContentValues();
                                values.put(TaskContract.TaskEntry.COL_TASK_TITLE, task);
                                db.insertWithOnConflict(TaskContract.TaskEntry.TABLE, null, values, SQLiteDatabase.CONFLICT_REPLACE);
                                db.close();
                                updateUI();
                            }
                        })
                        .setNegativeButton("Cancel", null).create();
                dialog.show();
                return true;

            default:
                return super.onOptionsItemSelected(item);
        }
    }
    // This method is created to delete a task and delete the view of the task in the screen
    // We are deleting the task form the database too.
    public void deleteTask(View view) {
        View parent = (View) view.getParent();
        TextView taskTextView = (TextView) parent.findViewById(R.id.title_task);
        String task = String.valueOf(taskTextView.getText());
        SQLiteDatabase db = taskHelper.getWritableDatabase();
        db.delete(TaskContract.TaskEntry.TABLE, TaskContract.TaskEntry.COL_TASK_TITLE + " = ?", new String[]{task});
        db.close();
        updateUI();
    }
    // This method is to update the task. We are using if and else to determine if the4 task needs
    // to be updated or if we clear them.
    private void updateUI() {
        ArrayList<String> taskList = new ArrayList<>();
        SQLiteDatabase db = taskHelper.getReadableDatabase();
        Cursor cursor = db.query(TaskContract.TaskEntry.TABLE,
                new String[]{TaskContract.TaskEntry._ID, TaskContract.TaskEntry.COL_TASK_TITLE},
                null, null, null, null, null);
        while (cursor.moveToNext()) {
            int idx = cursor.getColumnIndex(TaskContract.TaskEntry.COL_TASK_TITLE);
            taskList.add(cursor.getString(idx));
        }

        if (arrAdapter == null) {
            arrAdapter = new ArrayAdapter<>(this, R.layout.todo_task, R.id.title_task, taskList);
            TaskList.setAdapter(arrAdapter);
        } else {
            arrAdapter.clear();
            arrAdapter.addAll(taskList);
            arrAdapter.notifyDataSetChanged();
        }

        cursor.close();
        db.close();
    }
}
