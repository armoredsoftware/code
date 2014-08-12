package codepackage;

import java.util.List;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class EvidencePiece {
	private long[] mRep;
	private String tag;
	private String mReptag;

	public EvidencePiece(JSONObject jObj) {
		setTag((String) (jObj.get("tag")));
		List<Long> lst=null;
		mReptag= chooseRepTag(tag);
		
		lst = (List<Long>) jObj.get(mReptag);
		mRep = new long[lst.size()];
		for (int i = 0; i < lst.size(); i++) {
			mRep[i] = (long) lst.get(i);
		}
		// setM0Rep();

	}

	private String chooseRepTag(String tag_) {
		if (tag_.compareTo("M0") == 0) {
			return "m0Rep_EvidencePiece";
			
		}else if (tag_.compareTo("M1") == 0) {
			return "m1Rep_EvidencePiece";
		}else if (tag_.compareTo("M2") == 0) {
			return "m2Rep_EvidencePiece";
		}
		return null;
	}

	public EvidencePiece(long[] data, String constructor) {
		mRep = data;
		tag = constructor;
		mReptag= chooseRepTag(tag);

	}

	public long[] getM0Rep() {
		return this.mRep;
	}

	public void setM0Rep(long[] m0Rep) {
		this.mRep = m0Rep;
	}

	public String getTag() {
		return this.tag;
	}

	public void setTag(String tag) {
		this.tag = tag;
	}

	public JSONObject jsonEncode() {
		JSONObject jObj = new JSONObject();
		jObj.put("tag", tag);
		JSONArray jarray = new JSONArray();
		for (int i = 0; i < mRep.length; i++) {
			jarray.add(mRep[i]);
		}
		
		jObj.put(mReptag, jarray);
		JSONObject wrapper = new JSONObject();
		wrapper.put("tag", "EvidencePieceW");
		wrapper.put("getEvidencePiece", jObj);
		return wrapper;

	}
}
