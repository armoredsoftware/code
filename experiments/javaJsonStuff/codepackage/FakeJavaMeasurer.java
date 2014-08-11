package codepackage;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.Scanner;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

public class FakeJavaMeasurer {
	public static boolean verbose = true;

	public static String prefix = "/users/paulkline/Documents/protocolRepo/protocol/demos/demo2/";
	public static String deFile = "haskell_out_desiredEvidence";
	public static String edFile = "haskell_out_evidenceDescriptorW";
	public static String epFile = "haskell_out_evidencePiece";

	public static int chan_val;

	public static void main(String args[]) {

		// String deStr = getContents(deFile).trim();
		// String edStr = getContents(edFile).trim();
		// String epStr = getContents(epFile).trim();
		//
		// if (verbose) {
		// System.out.println("deFile: " + deStr);
		// System.out.println("edFile: " + edStr);
		// System.out.println("epFile: " + epStr);
		// }
		// DesiredEvidence de = jsonDecode(deStr, DesiredEvidence.class);

		Scanner in = new Scanner(System.in);
		JVChanUtil vchanUtil = new JVChanUtil();
		System.out.println("Dom ID: " + vchanUtil.getDomId());
		// System.out.println("Server (1) or Client (0):");
		// int srv = in.nextInt();
		int srv = 1;

		System.out.println("Enter a Dom ID: ");
		chan_val = in.nextInt();
		long logger = vchanUtil.createLogger();
		long chan;
		if (srv == 1) {
			chan = vchanUtil.server_init(logger, chan_val);
		} else {
			chan = vchanUtil.client_init(logger, chan_val);
			String mesg = "Testing vchan JNI";
			System.out.println("Sending: " + mesg);
			vchanUtil.sendChunkedMessage(logger, chan, mesg, mesg.length());
		}
		if (srv == 1) {
			while (true) {
				chan = vchanUtil.server_init(logger, chan_val);//this go here??
				vchanUtil.ctrlWait(chan);

				String message = vchanUtil.readChunkedMessage(logger, chan);
				// test to discard null character
				message = cStringToJavaString(message);
				System.out.println("Received: " + message);
				processReceivedMessage(message);
			}

		}

	}

	private static String cStringToJavaString(String message) {
		char[] chars = message.toCharArray();
		char[] result = new char[chars.length];

		for (int i = 0; i < chars.length; i++) {
			result[i] = chars[i];
		}
		return new String(result);
	}

	private static void processReceivedMessage(String jsonmessage) {

		EvidenceDescriptor ed = generaljsonDecode(jsonmessage);// (jsonmessage,
		System.out.println("Here is the evidenceDescriptor: "
				+ ed.getEvidenceDescriptor());
		EvidencePiece response = null;
		if (ed.getEvidenceDescriptor().compareTo("D0") == 0) {
			response = new EvidencePiece(new long[] { 0 });

		} else if (ed.getEvidenceDescriptor().compareTo("D1") == 0) {
			response = new EvidencePiece(new long[] { 0, 1 });

		} else if (ed.getEvidenceDescriptor().compareTo("D2") == 0) {
			response = new EvidencePiece(new long[] { 0, 1, 2 });

		}
		send(response);
		// EvidenceDescriptor.class);
		// EvidencePiece ep = jsonDecode(epStr, EvidencePiece.class);
		// if (verbose) {
		// System.out.println(de);
		// System.out.println(de.getEvidenceDescriptorList());
		// System.out.println(ed);
		// System.out.println(ed.getEvidenceDescriptor());
		// System.out.println(ep);
		// System.out.println(ep.getTag());
		// System.out.println(ep.getM0Rep());
		// }
		// System.out.println("encoding:");
		// String deStr2 = jsonEncode(de, DesiredEvidence.class).toJSONString();
		// System.out
		// .println(deStr2 + " match: " + (deStr2.compareTo(deStr) == 0));
		//
		// System.out.println("here is the generic read of evidencePieceW:");
		// System.out.println(getContents("haskell_out_evidencePieceW").trim());
		// EvidencePiece ep2 = generaljsonDecode(getContents(
		// "haskell_out_evidencePieceW").trim());
		// System.out.println(ep2.jsonEncode());
		//
		// System.out.println("encoding fail test:");
		// System.out.println(jsonEncode(ed,
		// DesiredEvidence.class).toJSONString());

	}

	private static void send(EvidencePiece response) {
		JVChanUtil vchanUtil = new JVChanUtil();
		long logger = vchanUtil.createLogger();
		long chan;

		System.out.println("about to send message this is chan_val:" + chan_val);
		JSONObject jsonObj = response.jsonEncode();
		String mesg = jsonObj.toJSONString();
		System.out.println("Sending: " + mesg);
		chan = vchanUtil.client_init(logger, chan_val);
		
		vchanUtil.sendChunkedMessage(logger, chan, mesg, mesg.length());

	}

	private static <T> T generaljsonDecode(String jsonString) {
		JSONParser parser = new JSONParser();
		Object obj;
		JSONObject jObj = null;

		try {
			obj = parser.parse(jsonString);
			System.out.println(obj);
			jObj = (JSONObject) obj;

			String unwrappedjsonStringObj = "";
			if (jObj.get("tag").toString().compareTo("EvidencePieceW") == 0) {
				unwrappedjsonStringObj = jObj.get("getEvidencePiece")
						.toString();
				return (T) jsonDecode(unwrappedjsonStringObj,
						EvidencePiece.class);
			} else if (jObj.get("tag").toString()
					.compareTo("EvidenceDescriptorW") == 0) {
				// unwrappedjsonStringObj =
				// jObj.get("getEvidenceDescriptor").toString();
				// too much unwrapping. trying without unwrapping
				return (T) jsonDecode(jsonString, EvidenceDescriptor.class);
			}

		} catch (ParseException e) {
			e.printStackTrace();
		}

		return null;
	}

	private static <T> T jsonDecode(String jsonStr, Class<T> class1) {
		JSONParser parser = new JSONParser();
		Object obj;
		JSONObject jObj = null;

		try {
			System.out.println("I am about to parse this: " + jsonStr + " as "
					+ class1);
			obj = parser.parse(jsonStr);
			System.out.println(obj);
			jObj = (JSONObject) obj;

		} catch (ParseException e) {
			e.printStackTrace();
		}

		// case DesirecEvidence
		if (class1 == DesiredEvidence.class) {
			DesiredEvidence de = new DesiredEvidence(jObj);
			return (T) de;
		}

		// case EvidencePiece
		if (class1 == EvidencePiece.class) {
			EvidencePiece ep = new EvidencePiece(jObj);
			return (T) ep;
		}

		// case EvidenceDescriptor
		if (class1 == EvidenceDescriptor.class) {
			EvidenceDescriptor ed = new EvidenceDescriptor(jObj
					.get("getEvidenceDescriptor").toString().trim());
			return (T) ed;
		}

		// failure
		return null;

	}

	private static <T> JSONObject jsonEncode(Object obj, Class<T> class1) {

		JSONObject jObj = new JSONObject();

		// case DesirecEvidence
		if (class1 == DesiredEvidence.class) {
			try {
				DesiredEvidence de = (DesiredEvidence) obj;
				jObj.put("evidenceDescriptorList",
						de.getEvidenceDescriptorList());
			} catch (Exception e) {
				// TODO Auto-generated catch block
				throw (new Error(e.getStackTrace().toString()
						+ "Error: perhaps mismatched obj and class"));
			}

			return jObj;
		}

		// case EvidencePiece
		if (class1 == EvidencePiece.class) {
			EvidencePiece ep = (EvidencePiece) obj;
			jObj.put("tag", ep.getTag());
			jObj.put("m0Rep", ep.getM0Rep());
			return jObj;
		}

		// case EvidenceDescriptor
		if (class1 == EvidenceDescriptor.class) {
			EvidenceDescriptor ed = (EvidenceDescriptor) obj;
			jObj.put("evidenceDescriptor", ed.getEvidenceDescriptor());
			return jObj;
		}

		// failure
		throw (new Error("No matching class found for encoding"));

	}

	// private static String getContents(String fname) {
	//
	// try {
	// BufferedReader br = new BufferedReader(new FileReader(prefix
	// + fname));
	//
	// StringBuilder sb = new StringBuilder();
	// String line = br.readLine();
	//
	// while (line != null) {
	// sb.append(line);
	// sb.append(System.lineSeparator());
	// line = br.readLine();
	// }
	//
	// br.close();
	// return sb.toString();
	// } catch (FileNotFoundException e) {
	// // TODO Auto-generated catch block
	// e.printStackTrace();
	// } catch (IOException e) {
	// // TODO Auto-generated catch block
	// e.printStackTrace();
	// }
	// return "";
	//
	// }

}
