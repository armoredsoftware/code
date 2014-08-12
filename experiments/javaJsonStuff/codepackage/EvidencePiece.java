package codepackage;

import java.util.List;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class EvidencePiece{
   	private long[] m0Rep;
   	private String tag;

 	public EvidencePiece(JSONObject jObj) {
		setTag((String)(jObj.get("tag")));

		List<Long> lst = (List<Long>)jObj.get("m0Rep_EvidencePiece");
		if (lst == null) {
			lst = (List)jObj.get("m0Rep");
		}
		m0Rep = new long[lst.size()];
		for (int i = 0; i < lst.size(); i++) {
			m0Rep[i] = (long) lst.get(i);
		}
		//setM0Rep();
		
	}
 	public EvidencePiece(long[] data, String constructor){
 		m0Rep=data;
 		tag=constructor;
 		
 	}
	public long[] getM0Rep(){
		return this.m0Rep;
	}
	public void setM0Rep(long[] m0Rep){
		this.m0Rep = m0Rep;
	}
 	public String getTag(){
		return this.tag;
	}
	public void setTag(String tag){
		this.tag = tag;
	}
	
	public JSONObject jsonEncode(){
		JSONObject jObj = new JSONObject();
		jObj.put("tag", tag);
		JSONArray jarray = new JSONArray();
		for (int i = 0; i < m0Rep.length; i++) {
			jarray.add(m0Rep[i]);
		}
		
		jObj.put("m0Rep_EvidencePiece",jarray);
		JSONObject wrapper = new JSONObject();
		wrapper.put("tag", "EvidencePieceW");
		wrapper.put("evidencePiece", jObj);
		return wrapper;
		
		
		
		
	}
}
