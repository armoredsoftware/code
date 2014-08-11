package codepackage;
import java.util.List;

import org.json.simple.JSONObject;

public class DesiredEvidence{
	
	public DesiredEvidence(JSONObject jObj){
		setEvidenceDescriptorList(
				(List)jObj.get("evidenceDescriptorList"));
		
		
	}
   	private List evidenceDescriptorList;

 	public List getEvidenceDescriptorList(){
		return this.evidenceDescriptorList;
	}
	public void setEvidenceDescriptorList(List evidenceDescriptorList){
		this.evidenceDescriptorList = evidenceDescriptorList;
	}
}
