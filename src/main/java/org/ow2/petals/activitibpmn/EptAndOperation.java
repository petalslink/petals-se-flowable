/**
 * Copyright (c) 2014-2014 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn;

public class EptAndOperation {
	
	private final String eptName;
	private final String operationName;
	

	public EptAndOperation(String eptName, String operationName) {
		this.eptName = eptName;
		this.operationName = operationName;
	}


	public String getEptName() {
		return eptName;
	}


	public String getOperationName() {
		return operationName;
	}


	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((eptName == null) ? 0 : eptName.hashCode());
		result = prime * result
				+ ((operationName == null) ? 0 : operationName.hashCode());
		return result;
	}


	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		EptAndOperation other = (EptAndOperation) obj;
		if (eptName == null) {
			if (other.eptName != null)
				return false;
		} else if (!eptName.equals(other.eptName))
			return false;
		if (operationName == null) {
			if (other.operationName != null)
				return false;
		} else if (!operationName.equals(other.operationName))
			return false;
		return true;
	}

}
