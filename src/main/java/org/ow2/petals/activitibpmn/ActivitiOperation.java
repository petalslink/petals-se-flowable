/**
 * Copyright (c) 2014 Linagora
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
import java.util.Map.Entry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.activiti.bpmn.model.FormProperty;
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
	    
    public ActivitiOperation(final String processDefinitionId, final String processKey, final String bpmnAction,
            final BpmnActionType bpmnActionType, final Properties bpmnProcessId, final Properties bpmnUserId,
            final Properties bpmnVarInMsg, final Properties outMsgBpmnVar, final Properties faultMsgBpmnVar,
            final Map<String, org.activiti.bpmn.model.FormProperty> bpmnVarType) {
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
    public void log(final Logger logger, final Level logLevel) {
        if (logger.isLoggable(logLevel)) {
            logger.log(logLevel, "processDefinitionId = " + this.processDefinitionId);
            logger.log(logLevel, "processKey = " + this.processKey);
            logger.log(logLevel, "bpmnAction = " + this.bpmnAction);
            logger.log(logLevel, "bpmnActionType = " + this.bpmnActionType);
            logger.log(
                    logLevel,
                    "bpmnProcessId: InMsg = " + this.bpmnProcessId.getProperty("inMsg") + " | outMsg = "
                            + this.bpmnProcessId.getProperty("outMsg") + " | faultMsg = "
                            + this.bpmnProcessId.getProperty("faultMsg"));
            logger.log(
                    logLevel,
                    "bpmnUserId: InMsg = " + this.bpmnUserId.getProperty("inMsg") + " | outMsg = "
                            + this.bpmnUserId.getProperty("outMsg") + " | faultMsg = "
                            + this.bpmnUserId.getProperty("faultMsg"));
            for (final Entry<Object, Object> entry : this.bpmnVarInMsg.entrySet()) {
                final String key = (String) entry.getKey();
                logger.log(logLevel, "bpmnVar => inMsg: " + key + " => " + entry.getValue());
            }
            for (final Entry<Object, Object> entry : this.outMsgBpmnVar.entrySet()) {
                final String key = (String) entry.getKey();
                logger.log(logLevel, "outMsg => bpmnVar: " + key + " => " + entry.getValue());
            }
            for (final Entry<Object, Object> entry : this.faultMsgBpmnVar.entrySet()) {
                final String key = (String) entry.getKey();
                logger.log(logLevel, "faultMsg => bpmnVar: " + key + " => " + entry.getValue());
            }
            logger.log(logLevel, "=== Activiti variable types");
            for (final Entry<String, FormProperty> entry : this.bpmnVarType.entrySet()) {
                final String key = entry.getKey();
                final FormProperty value = entry.getValue();
                logger.log(logLevel,
                        "bpmn variable : " + key + " - Name = " + value.getName() + " - Type = " + value.getType());
                if (value.getType().equals("enum")) {
                    for (final FormValue enumValue : value.getFormValues())
                        logger.log(logLevel, "|------  enum value Id = " + enumValue.getId() + " - Value = "
                                + enumValue.getName());
                } else if (value.getType().equals("date")) {
                    logger.log(logLevel, "|------  Date pattern = " + value.getDatePattern());
                }
            }
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
