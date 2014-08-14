import java.util.Scanner;
public class JVChanUtil {
    static{
       System.loadLibrary("JVChanUtil");
    }   
    public native int getDomId();
    public native long createLogger();
    public native long client_init(long logger, int srvId);
    public native long server_init(long logger, int clientId);
    
    public native void ctrlWait(long chan);
    public native void ctrlClose(long chan);
    public native int sendChunkedMessage(long logger, long chan, String msg, int size);
    public native String readChunkedMessage(long logger, long chan);

    public static void main(String args[]){
     Scanner in = new Scanner(System.in);
     JVChanUtil vchanUtil = new JVChanUtil();
     System.out.println("Dom ID: "+ vchanUtil.getDomId());
     System.out.println("Server (1) or Client (0):");
     int srv = in.nextInt();

     System.out.println("Enter a Dom ID: ");
     int val = in.nextInt();
     long logger = vchanUtil.createLogger();
     long chan;
     if ( srv == 1){
       chan = vchanUtil.server_init(logger,val);
     }else{
       chan = vchanUtil.client_init(logger, val);
       String mesg = "Testing vchan JNI";
       System.out.println("Sending: "+mesg);
       vchanUtil.sendChunkedMessage(logger,chan,mesg,mesg.length()); 
     }
        if (srv == 1){
           vchanUtil.ctrlWait(chan);
           System.out.println("Received: "+vchanUtil.readChunkedMessage(logger,chan));       
        }
    }






}
