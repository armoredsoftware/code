package codepackage;
import java.util.List;

import org.json.simple.JSONObject;


public class EvidenceDescriptor{
   	private String evidenceDescriptor;

 	public EvidenceDescriptor(String string) {
		setEvidenceDescriptor(string);//.get("evidenceDescriptor"));
	}
	public String getEvidenceDescriptor(){
		return this.evidenceDescriptor;
	}
	public void setEvidenceDescriptor(String evidenceDescriptor){
		this.evidenceDescriptor = evidenceDescriptor;
	}
}

