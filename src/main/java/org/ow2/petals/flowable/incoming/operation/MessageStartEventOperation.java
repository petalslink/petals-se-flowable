/**
 * Copyright (c) 2017-2025 Linagora
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
package org.ow2.petals.flowable.incoming.operation;

import java.util.Map;
import java.util.logging.Logger;

import org.flowable.engine.HistoryService;
import org.flowable.engine.IdentityService;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.runtime.ProcessInstance;
import org.ow2.petals.flowable.incoming.operation.annotated.MessageStartEventAnnotatedOperation;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableUnsupportedTypeException;

import com.ebmwebsourcing.easycommons.uuid.SimpleUUIDGenerator;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * The operation to create a new instance of a process
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class MessageStartEventOperation extends StartEventOperation {

    /**
     * The start event message name on which the action must be realized on the BPMN process side
     */
    private final String startEventMessageName;

    /**
     * The tenant identifier in which the process definition is deployed
     */
    private final String tenantId;

    /**
     * @param annotatedOperation
     *            Annotations of the operation to create
     * @param identityService
     *            The identity service of the BPMN engine
     * @param runtimeService
     *            The runtime service of the BPMN engine
     * @param historyService
     *            The history service of the BPMN engine
     * @param simpleUUIDGenerator
     *            A UUID generator
     * @param jacksonObjectMapper
     *            String to JSON converter of Jackson library.
     * @param logger
     * @throws VariableUnsupportedTypeException
     *             A variable definition contains an unsupported type.
     */
    public MessageStartEventOperation(final MessageStartEventAnnotatedOperation annotatedOperation,
            final IdentityService identityService, final RuntimeService runtimeService,
            final HistoryService historyService, final SimpleUUIDGenerator simpleUUIDGenerator,
            final ObjectMapper jacksonObjectMapper, final Logger logger) throws VariableUnsupportedTypeException {
        super(annotatedOperation, identityService, runtimeService, historyService, simpleUUIDGenerator,
                jacksonObjectMapper, logger);
        this.startEventMessageName = annotatedOperation.getStartEventMessageName();
        this.tenantId = annotatedOperation.getTenantId();
    }

    @Override
    protected ProcessInstance createProcessInstance(final Map<String, Object> processVars)
            throws OperationProcessingException {

        return this.runtimeService.startProcessInstanceByMessageAndTenantId(this.startEventMessageName, processVars,
                this.tenantId);
    }

    @Override
    protected void logFlowableOperation() {
        super.logFlowableOperation();
        this.logger.fine("Flowable Start Event Message Name = " + this.startEventMessageName);
    }

}
