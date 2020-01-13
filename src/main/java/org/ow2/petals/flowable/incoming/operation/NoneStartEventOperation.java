/**
 * Copyright (c) 2017-2020 Linagora
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
import java.util.Properties;
import java.util.logging.Logger;

import org.flowable.engine.HistoryService;
import org.flowable.engine.IdentityService;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.runtime.ProcessInstance;
import org.ow2.petals.flowable.incoming.operation.annotated.NoneStartEventAnnotatedOperation;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingException;

import com.ebmwebsourcing.easycommons.uuid.SimpleUUIDGenerator;

/**
 * The operation to create a new instance of a process
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class NoneStartEventOperation extends StartEventOperation {

    /**
     * The message start event identifier on which the action must be realized on the BPMN process side
     */
    private final String noneStartEventId;

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
     * @param placeholders
     *            Placeholders defined at component level
     * @param logger
     */
    public NoneStartEventOperation(final NoneStartEventAnnotatedOperation annotatedOperation,
            final IdentityService identityService, final RuntimeService runtimeService,
            final HistoryService historyService, final SimpleUUIDGenerator simpleUUIDGenerator,
            final Properties placeholders, final Logger logger) {
        super(annotatedOperation, identityService, runtimeService, historyService, simpleUUIDGenerator, placeholders,
                logger);
        this.noneStartEventId = annotatedOperation.getNoneStartEventId();
    }

    @Override
    protected ProcessInstance createProcessInstance(final Map<String, Object> processVars)
            throws OperationProcessingException {

        // We use RuntimeService.startProcessInstanceById() to be able to create a process instance from the given
        // process version. As only one step 'none start event' can be put in process definition, it is not needed to
        // explicitly use its identifier.
        // TODO: Create a unit test where the process was undeployed without undeploying the service unit
        return this.runtimeService.startProcessInstanceById(this.deployedProcessDefinitionId, processVars);
    }

    @Override
    protected void logFlowableOperation() {
        super.logFlowableOperation();
        this.logger.fine("Flowable None Start Event Id = " + this.noneStartEventId);
    }

}
