package org.jjcl.syntax;

import java.util.Properties;

public abstract class AbstractCarte implements Carte {
	private String logicalName;
	private TypeCarte type;
	private Properties parameters;
	
	@Override
	public String getLogicalname() {
		return logicalName;
	}
	
	@Override
	public void addParameter(String key, String value) {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public TypeCarte getTypeCarte() {
		return type;
	}
}
