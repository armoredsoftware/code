package codepackage;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

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

	public static void fakeJavaMeasurerMain(String[] args) {

		String deStr = getContents(deFile).trim();
		String edStr = getContents(edFile).trim();
		String epStr = getContents(epFile).trim();

		if (verbose) {
			System.out.println("deFile: " + deStr);
			System.out.println("edFile: " + edStr);
			System.out.println("epFile: " + epStr);
		}
		DesiredEvidence de = jsonDecode(deStr, DesiredEvidence.class);
		EvidenceDescriptor ed = jsonDecode(edStr, EvidenceDescriptor.class);
		EvidencePiece ep = jsonDecode(epStr, EvidencePiece.class);
		if (verbose) {
			System.out.println(de);
			System.out.println(de.getEvidenceDescriptorList());
			System.out.println(ed);
			System.out.println(ed.getEvidenceDescriptor());
			System.out.println(ep);
			System.out.println(ep.getTag());
			System.out.println(ep.getM0Rep());
		}
		System.out.println("encoding:");
		String deStr2 = jsonEncode(de, DesiredEvidence.class).toJSONString();
		System.out.println(deStr2 + " match: " + (deStr2.compareTo(deStr) == 0));
		
		
		System.out.println("here is the generic read of evidencePieceW:");
		System.out.println(getContents("haskell_out_evidencePieceW").trim());
		EvidencePiece ep2 = generaljsonDecode(getContents("haskell_out_evidencePieceW").trim());
		System.out.println(ep2.jsonEncode());
//
//		System.out.println("encoding fail test:");
//		System.out.println(jsonEncode(ed, DesiredEvidence.class).toJSONString());

	}

	private static <T> T generaljsonDecode(String str1){
		JSONParser parser = new JSONParser();
		Object obj;
		JSONObject jObj = null;

		try {
			obj = parser.parse(str1);
			System.out.println(obj);
			jObj = (JSONObject) obj;
			
			if (jObj.get("tag").toString().compareTo("EvidencePieceW") == 0) {
				String str2 = jObj.get("evidencePiece").toString();
				return (T) jsonDecode(str2, EvidencePiece.class);
			}

		} catch (ParseException e) {
			e.printStackTrace();
		}
		
		return null;
	}
	private static <T> T jsonDecode(String str1, Class<T> class1) {
		JSONParser parser = new JSONParser();
		Object obj;
		JSONObject jObj = null;

		try {
			obj = parser.parse(str1);
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
			EvidenceDescriptor ed = new EvidenceDescriptor(jObj);
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
				jObj.put("evidenceDescriptorList", de.getEvidenceDescriptorList());
			} catch (Exception e) {
				// TODO Auto-generated catch block
				throw (new Error (e.getStackTrace().toString() + "Error: perhaps mismatched obj and class"));
			}
			
			return jObj;
		}

		// case EvidencePiece
		if (class1 == EvidencePiece.class) {
			EvidencePiece ep =  (EvidencePiece) obj;
			jObj.put("tag", ep.getTag());
			jObj.put("m0Rep", ep.getM0Rep());
			return jObj;
		}

		// case EvidenceDescriptor
		if (class1 == EvidenceDescriptor.class) {
			EvidenceDescriptor ed = (EvidenceDescriptor)obj;
			jObj.put("evidenceDescriptor", ed.getEvidenceDescriptor());
			return jObj;
		}

		// failure
		throw (new Error("No matching class found for encoding"));
		

	}

	private static String getContents(String fname) {

		try {
			BufferedReader br = new BufferedReader(new FileReader(prefix
					+ fname));

			StringBuilder sb = new StringBuilder();
			String line = br.readLine();

			while (line != null) {
				sb.append(line);
				sb.append(System.lineSeparator());
				line = br.readLine();
			}

			br.close();
			return sb.toString();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return "";

	}

}