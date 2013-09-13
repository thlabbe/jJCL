package org.jjcl.syntax;

import java.util.List;

public class CarteDd extends AbstractCarte {

	private List<String> dsns; // pour les concatenations
	private String physicalName;
	
	
	
	
	
	@Override
	public void addChild(Carte carte) throws JCLSyntaxException {
		if(carte==null) {
			throw new JCLSyntaxException(Messages.getString("Carte.0")); //$NON-NLS-1$
		} else if (TypeCarte.DD!=carte.getTypeCarte()) {
			throw new JCLSyntaxException("Une Carte " + carte.getTypeCarte() + " ne peut-être ajoutée à une carte " + this.getTypeCarte() ); //$NON-NLS-1$ //$NON-NLS-2$
		} else {
			this.dsns.add(((CarteDd) carte).getPhysicalName());
		}
	}

	public String getPhysicalName() {
		return physicalName;
	}

	
}
