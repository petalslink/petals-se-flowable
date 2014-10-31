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

import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.activiti.bpmn.model.FormValue;
import org.ow2.petals.activitibpmn.ActivitiSEConstants.BpmnActionType;

public class ActivitiOperation {

	private final String processDefinitionId;
	private final String processKey;
	private final String bpmnAction; 
	private final BpmnActionType bpmnActionType; 
	private final Properties bpmnProcessId;
	private final Properties bpmnUserId;
	private final Properties bpmnVarInMsg;
	private final Properties outMsgBpmnVar;
	private final Properties faultMsgBpmnVar;
	private final Map<String, org.activiti.bpmn.model.FormProperty > bpmnVarType;

	/**
	 * Constructor.
	 */
	    
	public ActivitiOperation (String processDefinitionId,
            				  String processKey,
            				  String bpmnAction,
			                  BpmnActionType bpmnActionType,
			                  Properties bpmnProcessId,
			                  Properties bpmnUserId,
			                  Properties bpmnVarInMsg,
			                  Properties outMsgBpmnVar,
			                  Properties faultMsgBpmnVar,
			                  Map<String, org.activiti.bpmn.model.FormProperty > bpmnVarType ) {
		this.processDefinitionId = processDefinitionId ;
		this.processKey = processKey ;
		this.bpmnAction = bpmnAction ;
		this.bpmnActionType = bpmnActionType ;
		this.bpmnProcessId = bpmnProcessId;
		this.bpmnUserId = bpmnUserId;
		this.bpmnVarInMsg = bpmnVarInMsg;
		this.outMsgBpmnVar = outMsgBpmnVar;
		this.faultMsgBpmnVar = faultMsgBpmnVar;
		this.bpmnVarType = bpmnVarType;
	}

	/**
	 * @param logLevel
	 */
	public void logActivitiOperation( Logger logger, Level logLevel) {
		logger.log(logLevel, "processDefinitionId = " + processDefinitionId);
		logger.log(logLevel, "processKey = " + processKey);
		logger.log(logLevel, "bpmnAction = " + bpmnAction);
		logger.log(logLevel, "bpmnActionType = " + bpmnActionType);
		logger.log(logLevel, "bpmnProcessId: InMsg = " + bpmnProcessId.getProperty("inMsg")
				+ " | outMsg = "+ bpmnProcessId.getProperty("outMsg")
				+ " | faultMsg = "+ bpmnProcessId.getProperty("faultMsg") );
		logger.log(logLevel, "bpmnUserId: InMsg = " + bpmnUserId.getProperty("inMsg")
				+ " | outMsg = "+ bpmnUserId.getProperty("outMsg")
				+ " | faultMsg = "+ bpmnUserId.getProperty("faultMsg") );
		for(String key : bpmnVarInMsg.stringPropertyNames()) 
			logger.log(logLevel, "bpmnVar => inMsg: " + key + " => " + bpmnVarInMsg.getProperty(key) );
		for(String key : outMsgBpmnVar.stringPropertyNames()) 
			logger.log(logLevel, "outMsg => bpmnVar: " + key + " => " + outMsgBpmnVar.getProperty(key) );
		for(String key : faultMsgBpmnVar.stringPropertyNames()) 
			logger.log(logLevel, "faultMsg => bpmnVar: " + key + " => " + faultMsgBpmnVar.getProperty(key) );
		logger.log(logLevel,"=== TYPE des VARIALBE ACTIVITI");
		for(String key : bpmnVarType.keySet()) {
			logger.log(logLevel,"bpmn variable : " + key + " - Name = " + bpmnVarType.get(key).getName()
					+ " - Type = " + bpmnVarType.get(key).getType()  );
			if ( bpmnVarType.get(key).getType().equals("enum") ) 
				for (FormValue enumValue : bpmnVarType.get(key).getFormValues() ) 
   				logger.log(logLevel,"|------  enum value Id = " + enumValue.getId() + " - Value = " + enumValue.getName()  );
			if ( bpmnVarType.get(key).getType().equals("date") )
				logger.log(logLevel,"|------  Date pattern = " + bpmnVarType.get(key).getDatePattern() );

		}
	}
	
   	public String getProcessDefinitionId() {
		return this.processDefinitionId;
	}

	public String getProcessKey() {
		return this.processKey;
	}

	public String getBpmnAction() {
		return this.bpmnAction;
	}

	public BpmnActionType getBpmnActionType() {
		return this.bpmnActionType;
	}

	public Properties getBpmnProcessId() {
		return this.bpmnProcessId;
	}

	public Properties getBpmnUserId() {
		return this.bpmnUserId;
	}

	public Properties getBpmnVarInMsg() {
		return this.bpmnVarInMsg;
	}

	public Properties getOutMsgBpmnVar() {
		return this.outMsgBpmnVar;
	}

	public Properties getFaultMsgBpmnVar() {
		return this.faultMsgBpmnVar;
	}

	public Map<String, org.activiti.bpmn.model.FormProperty > getBpmnVarType() {
		return this.bpmnVarType;
	}
	
}
