/**
 * Copyright (c) 2014-2015 Linagora
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
package org.ow2.petals.activitibpmn.operation;

import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;

import org.activiti.engine.IdentityService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.runtime.ProcessInstance;
import org.ow2.petals.activitibpmn.operation.annotated.AnnotatedOperation;
import org.ow2.petals.activitibpmn.operation.annotated.StartEventAnnotatedOperation;
import org.ow2.petals.activitibpmn.operation.exception.OperationProcessingException;

/**
 * The operation to create a new instance of a process
 * 
 * @author Bertrand ESCUDIE - Linagora
 * @author Christophe DENEUX - Linagora
 * 
 */
public class StartEventOperation extends ActivitiOperation {

    /**
     * The identity service of the BPMN engine
     */
    private final IdentityService identityService;

    /**
     * The runtime service of the BPMN engine
     */
    private final RuntimeService runtimeService;

    /**
     * @param annotatedOperation
     *            Annotations of the operation to create
     * @param identityService
     *            The identity service of the BPMN engine
     * @param runtimeService
     *            The runtime service of the BPMN engine
     * @param logger
     */
    public StartEventOperation(final AnnotatedOperation annotatedOperation, final IdentityService identityService,
            final RuntimeService runtimeService, final Logger logger) {
        super(annotatedOperation, logger);
        this.identityService = identityService;
        this.runtimeService = runtimeService;
    }

    @Override
    public String getAction() {
        return StartEventAnnotatedOperation.BPMN_ACTION;
    }

    @Override
    protected void doExecute(final DOMSource domSource, final String bpmnUserId,
            final Map<String, Object> processVars, final Map<QName, String> outputNamedValues)
 throws OperationProcessingException {

        // Start a new process instance
        // TODO Set the CategoryId (not automatically done, but automatically done for tenant_id ?)
        final String bpmnProcessIdValue;
        try {
            this.identityService.setAuthenticatedUserId(bpmnUserId);

            // We use RuntimeService.startProcessInstanceById() to be able to create a process instance from the given
            // process version.
            // TODO: Create a unit test where the process was undeployed without undeploying the service unit
            final ProcessInstance createdProcessInstance = this.runtimeService.startProcessInstanceById(
                    this.deployedProcessDefinitionId, processVars);
            bpmnProcessIdValue = createdProcessInstance.getId();
        } finally {
            this.identityService.setAuthenticatedUserId(null);
        }

        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.fine("*** NEW PROCESS INSTANCE started,  processId = " + bpmnProcessIdValue);
        }

        // To prepare the output response, we add named value dedicated to this operation:
        // - process instance variables
        // - the identifier of the created process instance
        // TODO: Are process instance variables different from the provided variables 'processVars' ? If no, don't
        // retrieve them and use directly 'processVars'
        final ProcessInstance retrievedProcessInstance = this.runtimeService.createProcessInstanceQuery()
                .processInstanceId(bpmnProcessIdValue).includeProcessVariables().singleResult();
        if (retrievedProcessInstance == null) {
            // This exception should not occur
            throw new OperationProcessingException(this.wsdlOperationName, String.format(
                    "The just created process instance '%s' is not found for the process definition '%s'.",
                    bpmnProcessIdValue, this.deployedProcessDefinitionId));
        }
        for (final Entry<String, Object> processVariable : retrievedProcessInstance.getProcessVariables().entrySet()) {
            outputNamedValues.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_PROCESS_INSTANCE_PARAMS,
                    processVariable.getKey()), this.convertBpmnVariableValueToXslParam(processVariable.getValue()));

        }
        outputNamedValues.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_SPECIAL_PARAMS,
                SCHEMA_OUTPUT_XSLT_PARAM_PROCESS_INSTANCE_ID), bpmnProcessIdValue);
        outputNamedValues.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_SPECIAL_PARAMS,
                SCHEMA_OUTPUT_XSLT_PARAM_USER_ID), bpmnUserId);
    }

}