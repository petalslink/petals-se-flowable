/**
 * Copyright (c) 2015-2018 Linagora
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
package org.ow2.petals.flowable.incoming.integration;

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_ACTIVATEPROCESSINSTANCES;

import java.net.URI;
import java.util.List;
import java.util.logging.Logger;

import org.flowable.engine.RuntimeService;
import org.flowable.engine.common.api.FlowableException;
import org.flowable.engine.common.api.FlowableObjectNotFoundException;
import org.ow2.petals.components.flowable.generic._1.ActivateProcessInstances;
import org.ow2.petals.components.flowable.generic._1.ActivateProcessInstancesResponse;
import org.ow2.petals.components.flowable.generic._1.ActivateProcessInstancesResponse.ProcessInstanceIdentifier;
import org.ow2.petals.components.flowable.generic._1.ActivationResult;
import org.ow2.petals.components.flowable.generic._1.InvalidRequest;
import org.ow2.petals.flowable.incoming.integration.exception.OperationInitializationException;

/**
 * The integration operation to activate a process instance list
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ActivateProcessInstancesOperation extends
        AbstractOperation<ActivateProcessInstances, ActivateProcessInstancesResponse> {

    public static final URI FAULT_ACTOR = URI
            .create("http://petals.ow2.org/components/flowable/generic/1.0/ActivateProcessInstances");

    /**
     * The Flowable task service
     */
    private final RuntimeService runtimeService;

    public ActivateProcessInstancesOperation(final RuntimeService runtimeService, final Logger log)
            throws OperationInitializationException {
        super(ITG_OP_ACTIVATEPROCESSINSTANCES, FAULT_ACTOR, new Class[] { ActivateProcessInstances.class,
                ActivateProcessInstancesResponse.class, InvalidRequest.class }, log);
        this.runtimeService = runtimeService;
    }

    @Override
    public ActivateProcessInstancesResponse doExecute(final ActivateProcessInstances incomingObject) throws Exception {

        final ActivateProcessInstancesResponse response = new ActivateProcessInstancesResponse();
        final List<ProcessInstanceIdentifier> results = response.getProcessInstanceIdentifier();
        for (final String processInstanceId : incomingObject.getProcessInstanceIdentifier()) {
            this.log.fine(String.format("Activates process instance #%s.", processInstanceId));
            final ProcessInstanceIdentifier result = new ProcessInstanceIdentifier();
            result.setValue(processInstanceId);
            results.add(result);

            try {
                this.runtimeService.activateProcessInstanceById(processInstanceId);
                result.setResult(ActivationResult.ACTIVATED);
            } catch (final FlowableObjectNotFoundException e) {
                result.setResult(ActivationResult.NOT_FOUND);
            } catch (final FlowableException e) {
                result.setResult(ActivationResult.ALREADY_ACTIVATED);
            }
        }

        return response;
    }

}
