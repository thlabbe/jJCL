package org.jjcl.syntax;

public interface Carte {
	public String getLogicalname();
	public TypeCarte getTypeCarte();
	public void addChild(Carte carte) throws JCLSyntaxException;
	public void addParameter(String key, String value);
}
