package com.example.unitconverter;

// Import libraries automatically.
import android.graphics.Color;
// import android.support.v7.app.AppCompatActivity; //
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;

// We are creating a class where we are going to use our converter //
public class MainActivity extends AppCompatActivity {

    //We are creating the function here and we are linking the layout too.
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // We are creating the buttons and the linking in the layout.
        Button mbutton;
        final EditText mET;
        final TextView mTV;

        mbutton= (Button) findViewById(R.id.button);
        mET=(EditText) findViewById(R.id.editText);
        mTV=(TextView) findViewById(R.id.textView);

        // In this part we are creating the function when the user clicks then the formula is
        // going to be performed
        mbutton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Double convert= Double.parseDouble(mET.getText().toString());
                mTV.setText(String.valueOf(convert*100));               //Here is where the formula is located//
                mTV.setTextColor(Color.BLUE); //  Answer with color blue
            }
        });
    }
}