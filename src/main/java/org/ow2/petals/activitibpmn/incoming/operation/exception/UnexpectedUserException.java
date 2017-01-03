/**
 * Copyright (c) 2015-2017 Linagora
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
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.incoming.operation.exception;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.ow2.petals.activitibpmn.incoming.operation.ActivitiOperation;

/**
 * The user set to complete the user task is not the one expected by the BPMN engine, or is not a member of the group on
 * BPMN engine side.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class UnexpectedUserException extends OperationProcessingFault {

    private static final long serialVersionUID = 3656944296098424643L;

    private static final String MESSAGE_PATTERN = "Unexpected user to complete the task '%s' of the process instance '%s' by the BPMN engine";

    /**
     * The identifier of the completed task
     */
    private final String taskId;

    /**
     * The identifier of the process instance for which the task is completed
     */
    private final String processInstanceId;

    /**
     * The identifier of the unexpected user
     */
    private final String userId;

    /**
     * @param processInstanceId
     *            The identifier of the process instance not found
     * @param taskId
     *            The identifier of the process instance not found
     * @param userId
     *            The identifier of the unexpected user
     */
    public UnexpectedUserException(final QName wsdlOperation, final String processInstanceId, final String taskId,
            final String userId) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, taskId, processInstanceId));
        this.processInstanceId = processInstanceId;
        this.taskId = taskId;
        this.userId = userId;
    }

    /**
     * @return The identifier of the completed task
     */
    public String getTaskId() {
        return this.taskId;
    }

    /**
     * @return The identifier of the process instance not found
     */
    public String getProcessInstanceId() {
        return this.processInstanceId;
    }

    /**
     * @return The identifier of the unexpected user
     */
    public String getUserId() {
        return this.userId;
    }

    @Override
    public Map<QName, String> getXslParameters() {
        final Map<QName, String> xslParameters = new HashMap<>();
        xslParameters.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_FAULT_PARAMS,
                ActivitiOperation.SCHEMA_OUTPUT_XSLT_PARAM_PROCESS_INSTANCE_ID), this.processInstanceId);
        xslParameters.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_FAULT_PARAMS,
                ActivitiOperation.SCHEMA_OUTPUT_XSLT_PARAM_TASK_ID), this.taskId);
        xslParameters.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_FAULT_PARAMS,
                ActivitiOperation.SCHEMA_OUTPUT_XSLT_PARAM_USER_ID), this.userId);
        return xslParameters;
    }
}
