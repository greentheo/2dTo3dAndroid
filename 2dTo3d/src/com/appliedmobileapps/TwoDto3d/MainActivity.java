package com.appliedmobileapps.TwoDto3d;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Set;

import android.hardware.Camera;
import android.hardware.Camera.CameraInfo;
import android.hardware.Camera.PictureCallback;
import android.os.Bundle;
import android.os.Environment;
import android.app.Activity;
import android.content.pm.PackageManager;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.Toast;

public class MainActivity extends Activity implements PictureCallback {

	private final static String TAG = MainActivity.class.getSimpleName();
	
	private ImageView mainImage;
	private Button takePictureBtn;
	
	private Camera camera = null;
	private int cameraId = 0;
	
	Set<String> imageFilesList;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);
		
		
		
		mainImage = (ImageView)findViewById(R.id.mainImage);
		
		takePictureBtn = (Button)findViewById(R.id.pictureBtn);
		takePictureBtn.setOnClickListener(new OnClickListener(){

			@Override
			public void onClick(View arg0) {

				//Take Picture!
				if(camera!=null)
				{
					camera.takePicture(null, null, MainActivity.this);//new PhotoHandler(getApplicationContext()));
				}
				
			}
			
		});
	}
	
	@Override
	protected void onResume()
	{
		super.onResume();
		
		if(camera == null)
		{
			if (!getPackageManager().hasSystemFeature(PackageManager.FEATURE_CAMERA)) 
			{
				Toast.makeText(this, "No camera on this device", Toast.LENGTH_LONG).show();
			}
			else
			{
				cameraId = findBackFacingCamera();
				
				if (cameraId < 0)
				{
					Toast.makeText(this, "No back facing camera found.", Toast.LENGTH_LONG).show();
				} 
				else 
				{
					camera = Camera.open(cameraId);
				}
			}
		}
	}
	
	@Override
	protected void onPause() 
	{
		if (camera != null) 
		{
			camera.release();
			camera = null;
		}
		super.onPause();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.main, menu);
		return true;
	}
	
	
	private int findBackFacingCamera() 
	{
		int cameraId = -1;
		// Search for the back facing camera
		int numberOfCameras = Camera.getNumberOfCameras();
		for (int i = 0; i < numberOfCameras; i++) 
		{
			CameraInfo info = new CameraInfo();
			Camera.getCameraInfo(i, info);
			if (info.facing == CameraInfo.CAMERA_FACING_BACK) {
				Log.d(TAG, "Camera found");
				cameraId = i;
				break;
			}
		}
		return cameraId;
	}

	@Override
	public void onPictureTaken(byte[] data, Camera camera) 
	{

		File pictureFileDir = getDir();

		if (!pictureFileDir.exists() && !pictureFileDir.mkdirs()) 
		{
			Log.d(TAG, "Can't create directory to save image.");
			
			Toast.makeText(this, "Can't create directory to save image.", Toast.LENGTH_LONG).show();
			return;

		}

		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyymmddhhmmss", Locale.US);
		String date = dateFormat.format(new Date());
		String photoFile = "Picture_" + date + ".jpg";

		String filename = pictureFileDir.getPath() + File.separator + photoFile;

		File pictureFile = new File(filename);

		FileOutputStream fos = null;
		
		try {
			fos = new FileOutputStream(pictureFile);
			fos.write(data);
			
			imageFilesList.add(filename);
			
			
			Toast.makeText(this, "New Image saved:" + photoFile, Toast.LENGTH_LONG).show();
		} 
		catch (Exception error) 
		{
			Log.d(TAG, "File" + filename + "not saved: " + error.getMessage());

			Toast.makeText(this, "Image could not be saved.", Toast.LENGTH_LONG).show();
		}
		finally
		{
			if(fos!=null)
			{
				try {
					fos.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}
	}
	
	private File getDir() 
	{
		File sdDir = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES);
		return new File(sdDir, "TwoDTo3D_Picts");
	}
}
