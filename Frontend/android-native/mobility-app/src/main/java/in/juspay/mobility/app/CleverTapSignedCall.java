package in.juspay.mobility.app;

import static android.Manifest.permission.BLUETOOTH_CONNECT;
import static android.Manifest.permission.POST_NOTIFICATIONS;
import static android.Manifest.permission.RECORD_AUDIO;

import android.Manifest;
import android.app.AlertDialog;
import android.bluetooth.BluetoothAdapter;
import android.content.Context;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.provider.Settings;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.JavascriptInterface;
import android.widget.Button;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.cardview.widget.CardView;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

import com.clevertap.android.sdk.CleverTapAPI;
import com.clevertap.android.sdk.inapp.CTLocalInApp;
import com.clevertap.android.signedcall.enums.VoIPCallStatus;
import com.clevertap.android.signedcall.exception.CallException;
import com.clevertap.android.signedcall.exception.InitException;
import com.clevertap.android.signedcall.init.SignedCallAPI;
import com.clevertap.android.signedcall.init.SignedCallInitConfiguration;
import com.clevertap.android.signedcall.interfaces.OutgoingCallResponse;
import com.clevertap.android.signedcall.interfaces.SCVoIPCallStatusListener;
import com.clevertap.android.signedcall.interfaces.SignedCallInitResponse;
import com.clevertap.android.signedcall.models.CallDetails;
import com.clevertap.android.signedcall.models.MissedCallAction;
import com.clevertap.android.signedcall.models.SCCallStatusDetails;
import com.clevertap.android.signedcall.models.SignedCallScreenBranding;
import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.google.android.material.bottomsheet.BottomSheetDialog;
import com.google.firebase.analytics.FirebaseAnalytics;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;


public class CleverTapSignedCall {

    private final Context context;
    private final Activity activity;

    public static String phone;
    private BottomSheetDialog bottomSheetDialog;

    public static final int REQUEST_CALL = 8;
    public static final int REQUEST_MICROPHONE = 9;
    public static final int REQUEST_BLUETOOTH = 11;
    private static final int REQUEST_CODE_NOTIFICATION_PERMISSION = 10;

    public CleverTapSignedCall(Context cxt, Activity act){
        this.context = cxt;
        this.activity = act;
    }
    public CleverTapSignedCall(Context cxt, Activity act, boolean listenerOn){
        this.context = cxt;
        this.activity = act;
        if(listenerOn)signedCallListener();
    }

    public static int useFallbackDialer = 0;
    public static int callAttempts = 0;


    public void showDialer(String phoneNum) {
       Intent intent = new Intent(Intent.ACTION_DIAL);
       intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
       intent.setData(Uri.parse("tel:" + phoneNum));
       context.startActivity(intent);
    }

    protected boolean checkAudioPermission (){
        // if (Build.VERSION.SDK_INT < 30) {
        if (activity != null && ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
            return false;
        } else {
            return true;
        }
    }

    protected boolean checkAndAskBluetoothPermission (){
        if (Build.VERSION.SDK_INT < 30) {
            if (activity != null && ActivityCompat.checkSelfPermission(context, BLUETOOTH_CONNECT) != PackageManager.PERMISSION_GRANTED) {
                ActivityCompat.requestPermissions(activity, new String[]{BLUETOOTH_CONNECT}, REQUEST_BLUETOOTH);
                return false;
            } else {
                return true;
            }
        }
        return true;
    }

    public void voipDialer(String cuid, boolean isDriver, String phoneNum, boolean isMissed) {
        phone = phoneNum;
        System.out.println("Signedcall useFallbackDialer " + useFallbackDialer + " callattempts " + callAttempts);
        System.out.println("Signedcall voip" + useFallbackDialer + "phone" + phoneNum);   
        if(useFallbackDialer > 1 || callAttempts > 3){
            showDialer(phone);
            return;
        }
        BluetoothAdapter bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
        String receiverCuid, callContext, remoteContext;
        String ny_customer = context.getString(R.string.namma_yatri_customer);
        String ny_driver = context.getString(R.string.namma_yatri_driver);
        JSONObject callOptions = new JSONObject();
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        Bundle bundle = new Bundle();
        bundle.putString(isMissed ? "missed_voip_callback" : "ride_id", cuid);
        
        if (!isMissed) {
            cuid = cuid.replace("-", "");
            receiverCuid = (isDriver ? "customer" : "driver") + cuid;
        } else receiverCuid = cuid;
        
        callContext = isDriver
                ? (ny_customer.isEmpty() ? "Namma Yatri Customer" : ny_customer)
                : (ny_driver.isEmpty() ? "Namma Yatri Driver" : ny_driver);
        
        remoteContext = isDriver
                ? (ny_driver.isEmpty() ? "Namma Yatri Driver" : ny_driver)
                : (ny_customer.isEmpty() ? "Namma Yatri Customer" : ny_customer);
        
        try {
            callOptions.put("remote_context", remoteContext);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        String tempPhone = phone;
        OutgoingCallResponse outgoingCallResponseListener = new OutgoingCallResponse() {
            @Override
            public void onSuccess() {
                mFirebaseAnalytics.logEvent("voip_call_success",bundle);
                System.out.println("signedcall success me");
            }
            @Override
            public void onFailure(CallException callException) {
                System.out.println("signedcall failure me");
                Log.d("SignedCall: ", "error code: " + callException.getErrorCode()
                    + "\n error message: " + callException.getMessage()
                    + "\n error explanation: " + callException.getExplanation());

                if(callException.getErrorCode() == CallException.CanNotProcessCallRequest.getErrorCode()){
                    // Do nothing (user trying to spam call button, sdk still processing first click)
                } else if(callException.getErrorCode() == CallException.MicrophonePermissionNotGrantedException.getErrorCode()) {
                    useFallbackDialer++;
                    if (activity != null && ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
                        ActivityCompat.requestPermissions(activity, new String[]{RECORD_AUDIO}, REQUEST_MICROPHONE);
                    }
                    mFirebaseAnalytics.logEvent("voip_call_failed_NO_MIC_PERM_CALLER",bundle);
                    // Toast.makeText(context, "Enable Microphone Permission for Internet Calling.", Toast.LENGTH_LONG).show();
                } else if(callException.getErrorCode() == CallException.BadNetworkException.getErrorCode() || callException.getErrorCode() == CallException.NoInternetException.getErrorCode() || callException.getErrorCode() == CallException.ContactNotReachableException.getErrorCode() || callException.getErrorCode() == CallException.CallFeatureNotAvailable.getErrorCode()){
                    System.out.println("Signed call: " + "Failed signed call " + tempPhone);
                    // Toast.makeText(context, "Bad Network going ahead with direct call.", Toast.LENGTH_LONG).show();
                    mFirebaseAnalytics.logEvent("voip_call_failed_BAD_NETWORK_CALLER",bundle);
                    useFallbackDialer += 2;
                    showDialer(tempPhone);
                } else if(callException.getErrorCode() == CallException.CallFeatureNotAvailable.getErrorCode()) {
                    mFirebaseAnalytics.logEvent("voip_call_failed_INIT_NOT_DONE",bundle);
                    showDialer(tempPhone);
                } else {
                    useFallbackDialer+=2;
                    showDialer(tempPhone);
                    // Toast.makeText(context, "Call failed. Please try again later.", Toast.LENGTH_LONG).show();
                    mFirebaseAnalytics.logEvent("voip_call_failed_CALLER",bundle);
                }
            };
        };

        if(activity != null && checkAudioPermission()){
            if((bluetoothAdapter == null || !bluetoothAdapter.isEnabled())){
                SignedCallAPI.getInstance().call(context, receiverCuid, callContext, callOptions, outgoingCallResponseListener);
            } else if(checkAndAskBluetoothPermission()){
                SignedCallAPI.getInstance().call(context, receiverCuid, callContext, callOptions, outgoingCallResponseListener);
            } else {
                Log.e("signed call error :", "Bluetooth Permission not given" );
            }
        } else {
            mFirebaseAnalytics.logEvent("voip_call_failed_NO_MIC_PERM_CALLER",bundle);
            if (activity != null && ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
                ActivityCompat.requestPermissions(activity, new String[]{RECORD_AUDIO}, REQUEST_MICROPHONE);
            }
            String permissionStatus = getPermissionStatus(RECORD_AUDIO);
            if(permissionStatus == "DISABLED" || "DENIED" == permissionStatus)
                showCustomBottomSheet(isDriver);
            Log.e("signed call error :", "Signed call Audio Permission not given" );
             // NotificationUtils.firebaseLogEventWithParams(context, "voip_call_success", "ride_id", cuid);
        }
    }

    public String getPermissionStatus(String permission) {
        if (activity != null && ActivityCompat.checkSelfPermission(context, permission) == PackageManager.PERMISSION_DENIED) {
            if (ActivityCompat.shouldShowRequestPermissionRationale(activity, permission)) {
                return "DISABLED";
            } else {
                return "DENIED";
            }
        } else return "ENABLED";
    }

    public void showAlertForMicrophonePermission() {
    System.out.println("inside showAlertForMicrophonePermission");
    if (context == null || activity == null) {
        Log.e("PermissionDialog", "Context or Activity is null, cannot show dialog");
        return;
    }
    // Create an AlertDialog Builder
    AlertDialog.Builder builder = new AlertDialog.Builder(activity);
    builder.setCancelable(false); // Prevent dismissing the dialog by touching outside

    // Inflate the custom layout
    LayoutInflater inflater = activity.getLayoutInflater();

    ConstraintLayout constraintLayout = (ConstraintLayout) inflater.inflate(
        in.juspay.mobility.app.R.layout.microphone_permission_dialog_2, null
    );

    // Customize the layout's views (optional)
    CardView cardView = constraintLayout.findViewById(in.juspay.mobility.app.R.id.dialogCardView2);
    if (cardView != null) {
        cardView.setCardElevation(8); // Example customization
        cardView.setRadius(16);
    }

    // Set layout params if needed
    ViewGroup.LayoutParams layoutParams = new ConstraintLayout.LayoutParams(
        ConstraintLayout.LayoutParams.MATCH_PARENT,
        ConstraintLayout.LayoutParams.WRAP_CONTENT
    );
    constraintLayout.setLayoutParams(layoutParams);

    // Set the inflated layout as the dialog view
    builder.setView(constraintLayout);

    // Handle positive button click
    builder.setPositiveButton("Go to Settings", (dialog, which) -> {
        Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
        Uri uri = Uri.fromParts("package", activity.getPackageName(), null);
        intent.setData(uri);
        activity.startActivity(intent);
    });

    // Handle negative button click
    builder.setNegativeButton("Cancel", (dialog, which) -> dialog.cancel());

    // Run the dialog creation on the UI thread
    activity.runOnUiThread(() -> {
        AlertDialog alertDialog = builder.create();
        alertDialog.show();
    });
    }

    public void initSignedCall(String cuid, boolean isDriver){
        // We are initialising only at the time of ride booking 
        useFallbackDialer = 0;
        if (activity != null && ActivityCompat.checkSelfPermission(context, POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(activity, new String[]{POST_NOTIFICATIONS}, REQUEST_CODE_NOTIFICATION_PERMISSION);
            // showCustomBottomSheet(isDriver);
        }
        JSONObject initOptions = new JSONObject();
        String Cuid = (isDriver) ? "driver" + cuid : "customer" + cuid;
        Bundle bundle = new Bundle();
        bundle.putString("ride_id",cuid);
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        Cuid = Cuid.replace("-", "");
        try {
            initOptions.put("accountId", "6715f01d5b143458ec1c1200");
            initOptions.put("apiKey", "C82XhWtydCwnmFPRicYXcs9q6FUINbEiBCJBsUxhyB0fW1i5JOjkm2nTUwmGWV7j");
            initOptions.put("cuid", Cuid);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        final boolean[] onetry = {true};
        JSONObject jsonObjectConfig = CTLocalInApp.builder()
        .setInAppType(CTLocalInApp.InAppType.ALERT)
        .setTitleText("Get Notified")
        .setMessageText("Enable Notification permission")
        .followDeviceOrientation(true)
        .setPositiveBtnText("Allow")
        .setNegativeBtnText("Cancel")
        .build();
        SignedCallInitResponse signedCallInitListener = new SignedCallInitResponse() {
            @Override
            public void onSuccess() {
                Log.d("SignedCallIntitation: ", "Successfully initiated Signed Call (voip)");
                System.out.println("Signed call " + "Successfully initiated Signed Call (voip)");
                SignedCallAPI.setDebugLevel(SignedCallAPI.LogLevel.VERBOSE);
                mFirebaseAnalytics.logEvent("voip_init_success",bundle);

            }
            @Override
            public void onFailure(@NonNull InitException initException) {
                //App is notified on the main thread when the initialization is failed
                Log.d("SignedCall: ", "error code: " + initException.getErrorCode()
                        + "\n error message: " + initException.getMessage()
                        + "\n error explanation: " + initException.getExplanation());
                System.out.println("failed signed call init");
                mFirebaseAnalytics.logEvent("voip_init_failed",bundle);
                useFallbackDialer += 2;
            }
        };
        List<MissedCallAction> missedCallActionsList = new ArrayList<>();
        missedCallActionsList.add(new MissedCallAction("callback", "Callback"));
        missedCallActionsList.add(new MissedCallAction("dismiss", "Dismiss"));
        SignedCallScreenBranding callScreenBranding = new SignedCallScreenBranding(
            context.getString(R.string.voip_background), "#ffffff", context.getString(R.string.voip_logo), SignedCallScreenBranding.ButtonTheme.LIGHT, "#FF453A");
        callScreenBranding.setShowPoweredBySignedCall(false); //set false to hide the label from VoIP call screens. Default value is true. 

        SignedCallInitConfiguration initConfiguration = new SignedCallInitConfiguration.Builder(initOptions, false)
                                                            .setSwipeOffBehaviourInForegroundService(SignedCallInitConfiguration.SCSwipeOffBehaviour.PERSIST_CALL)
                                                            .overrideDefaultBranding(callScreenBranding)
                                                            .networkCheckBeforeOutgoingCallScreen(true)
                                                            .setMissedCallActions(missedCallActionsList)
                                                            .promptPushPrimer(jsonObjectConfig)
                                                            .setNotificationPermissionRequired(false)
                                                            .build();
        CleverTapAPI cleverTapAPI = CleverTapAPI.getDefaultInstance(context);
        SignedCallAPI.getInstance().init(context, initConfiguration, cleverTapAPI, signedCallInitListener);
    }

    public void destroySignedCall() {
        useFallbackDialer = 0;
        SignedCallAPI.getInstance().disconnectSignallingSocket(context);
        SignedCallAPI.getInstance().logout(context);
    }

    public void showCustomBottomSheet(boolean isDriver) {
        
    activity.runOnUiThread(() -> {

            if (activity == null || context == null) return;

            if (bottomSheetDialog != null && bottomSheetDialog.isShowing()) {
                return; 
            }
            // LayoutInflater inflater = LayoutInflater.from(activity);
            LayoutInflater inflater = activity.getLayoutInflater();

            int resourceValue = isDriver ? R.layout.bottom_sheet_permission_callmiss_driver : in.juspay.mobility.app.R.layout.bottom_sheet_permission;
            ConstraintLayout constraintLayout = (ConstraintLayout) inflater.inflate(resourceValue, null);
        
            bottomSheetDialog = new BottomSheetDialog(activity);
            bottomSheetDialog.setContentView(constraintLayout);
            bottomSheetDialog.setCancelable(false);
        
            Button btnGoToSettings = constraintLayout.findViewById(R.id.btn_go_to_settings);
            Button btnCancel = constraintLayout.findViewById(R.id.btn_cancel);
        
            btnGoToSettings.setOnClickListener(v -> {
                bottomSheetDialog.dismiss();
                Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
                Uri uri = Uri.fromParts("package", activity.getPackageName(), null);
                intent.setData(uri);
                activity.startActivity(intent);
            });
        
            btnCancel.setOnClickListener(v -> bottomSheetDialog.dismiss());
            bottomSheetDialog.show();
        });

    }
        
    

    public void signedCallListener(){
        SignedCallAPI.getInstance().registerVoIPCallStatusListener(new SCVoIPCallStatusListener() {
            @Override
            public void callStatus(final SCCallStatusDetails callStatusDetails) {
                //App is notified on the main thread to notify the changes in the call-state
                Log.d("signedcall", "callStatus is invoked with: " + callStatusDetails.toString());

                SCCallStatusDetails.CallDirection direction = callStatusDetails.getDirection();
                VoIPCallStatus callStatus = callStatusDetails.getCallStatus();
                CallDetails callDetails = callStatusDetails.getCallDetails();
                boolean isDriver = callDetails.calleeCuid.contains("driver");
                if (direction.equals(SCCallStatusDetails.CallDirection.OUTGOING)) {
                    //Handle events for initiator of the call
                    if (callStatus == VoIPCallStatus.CALL_IS_PLACED) {
                        // When the call is successfully placed
                    }  else if (callStatus == VoIPCallStatus.CALL_RINGING) {
                        // When the call starts ringing on the receiver's device
                    } else if (callStatus == VoIPCallStatus.CALL_CANCELLED) {
                        callAttempts++;
                        // When the call is cancelled from the initiator's end
                    } else if (callStatus == VoIPCallStatus.CALL_CANCELLED_DUE_TO_RING_TIMEOUT) {
                        callAttempts+=2;
                        // When the call is call is cancelled due to a ring timeout. 
                        // This event is reported when the SDK fails to establish communication with the receiver, often due to an offline device or a device with low bandwidth.
                    } else if (callStatus == VoIPCallStatus.CALL_DECLINED) {
                        callAttempts++;
                        // When the call is declined from the receiver's end
                    } else if (callStatus == VoIPCallStatus.CALL_MISSED) {
                        callAttempts++;
                        // When the call is missed at the receiver's end
                    } else if (callStatus == VoIPCallStatus.CALL_ANSWERED) {
                        // When the call is picked up by the receiver
                    } else if (callStatus == VoIPCallStatus.CALL_IN_PROGRESS) {
                        // When the connection to the receiver is established
                    } else if (callStatus == VoIPCallStatus.CALL_OVER) {
                        // When the call has been disconnected
                    } else if (callStatus == VoIPCallStatus.CALLEE_BUSY_ON_ANOTHER_CALL) {
                        // When the receiver is busy on another call(includes both VoIP or PSTN)
                    }  else if (callStatus == VoIPCallStatus.CALL_DECLINED_DUE_TO_BUSY_ON_VOIP) {
                        // When the receiver is busy in a VoIP call
                    } else if (callStatus == VoIPCallStatus.CALL_DECLINED_DUE_TO_BUSY_ON_PSTN) {
                        // When the receiver is busy in a PSTN call
                    } else if (callStatus == VoIPCallStatus.CALL_DECLINED_DUE_TO_LOGGED_OUT_CUID) {
                        useFallbackDialer+=2;
                        showDialer(phone);
                        // When the receiver's cuid is logged out and logged in with different cuid  
                    } else if (callStatus == VoIPCallStatus.CALL_DECLINED_DUE_TO_NOTIFICATIONS_DISABLED) {
                        useFallbackDialer+=2;
                        showDialer(phone);

                        // When the receiver's Notifications Settings are disabled from application settings
                    } else if (callStatus == VoIPCallStatus.CALLEE_MICROPHONE_PERMISSION_NOT_GRANTED) {
                        //fALLBACK TO EXOTEL - better would be next time call should go through exotel not this one
                       useFallbackDialer+=2;
                        showDialer(phone);
                        // When the Microphone permission is denied or blocked while receiver answers the call
                    } else if (callStatus == VoIPCallStatus.CALLEE_MICROPHONE_PERMISSION_BLOCKED) {
                        //fALLBACK TO EXOTEL
                       useFallbackDialer+=2;
                        showDialer(phone);
                        // When the microphone permission is blocked at the receiver's end.
                    } else if (callStatus == VoIPCallStatus.CALL_FAILED_DUE_TO_INTERNAL_ERROR) {
                        useFallbackDialer+=2;
                        showDialer(phone);

                        // if (context != null && ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
                        //     showCustomBottomSheet();
                        // }
                        // When the call fails after signalling. Possible reasons could include low internet connectivity, low RAM available on device, SDK fails to set up the voice channel within the time limit
                    }
                } else if (direction.equals(SCCallStatusDetails.CallDirection.INCOMING)) {
                        //Handle events for receiver of the call
                        if (callStatus == VoIPCallStatus.CALL_MISSED) {
                            // When the call is missed at the receiver's end
                            if (context != null && ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
                                showCustomBottomSheet(isDriver);
                            }
                        } else if (callStatus == VoIPCallStatus.CALLEE_MICROPHONE_PERMISSION_NOT_GRANTED) {
                            // When the Microphone permission is denied or blocked while receiver answers the call
                            if (context != null && ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
                                showCustomBottomSheet(isDriver);
                            }
                        } else if (callStatus == VoIPCallStatus.CALLEE_MICROPHONE_PERMISSION_BLOCKED) {
                            // When the microphone permission is blocked at the receiver's end.
                            if (context != null && ActivityCompat.checkSelfPermission(context, RECORD_AUDIO) != PackageManager.PERMISSION_GRANTED) {
                                showCustomBottomSheet(isDriver);
                            }
                        } 
                }
             }
        });
    }
}