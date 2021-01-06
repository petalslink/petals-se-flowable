/**
 * Copyright (c) 2015-2021 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_SUSPENDPROCESSINSTANCES;

import java.net.URI;
import java.util.List;
import java.util.logging.Logger;

import org.flowable.common.engine.api.FlowableException;
import org.flowable.common.engine.api.FlowableObjectNotFoundException;
import org.flowable.engine.RuntimeService;
import org.ow2.petals.components.flowable.generic._1.AdjournmentResult;
import org.ow2.petals.components.flowable.generic._1.InvalidRequest;
import org.ow2.petals.components.flowable.generic._1.SuspendProcessInstances;
import org.ow2.petals.components.flowable.generic._1.SuspendProcessInstancesResponse;
import org.ow2.petals.components.flowable.generic._1.SuspendProcessInstancesResponse.ProcessInstanceIdentifier;
import org.ow2.petals.flowable.incoming.integration.exception.OperationInitializationException;

/**
 * The integration operation to suspend a process instance list
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class SuspendProcessInstancesOperation extends
        AbstractOperation<SuspendProcessInstances, SuspendProcessInstancesResponse> {

    public static final URI FAULT_ACTOR = URI
            .create("http://petals.ow2.org/components/flowable/generic/1.0/SuspendProcessInstances");

    /**
     * The Flowable task service
     */
    private final RuntimeService runtimeService;

    public SuspendProcessInstancesOperation(final RuntimeService runtimeService, final Logger log)
            throws OperationInitializationException {
        super(ITG_OP_SUSPENDPROCESSINSTANCES, FAULT_ACTOR, new Class[] { SuspendProcessInstances.class,
                SuspendProcessInstancesResponse.class, InvalidRequest.class }, log);
        this.runtimeService = runtimeService;
    }

    @Override
    public SuspendProcessInstancesResponse doExecute(final SuspendProcessInstances incomingObject) throws Exception {

        final SuspendProcessInstancesResponse response = new SuspendProcessInstancesResponse();
        final List<ProcessInstanceIdentifier> results = response.getProcessInstanceIdentifier();
        for (final String processInstanceId : incomingObject.getProcessInstanceIdentifier()) {
            this.log.fine(String.format("Suspends process instance #%s.", processInstanceId));
            final ProcessInstanceIdentifier result = new ProcessInstanceIdentifier();
            result.setValue(processInstanceId);
            results.add(result);

            try {
                this.runtimeService.suspendProcessInstanceById(processInstanceId);
                result.setResult(AdjournmentResult.SUSPENDED);
            } catch (final @SuppressWarnings("squid:S1166") FlowableObjectNotFoundException e) {
                result.setResult(AdjournmentResult.NOT_FOUND);
            } catch (final @SuppressWarnings("squid:S1166") FlowableException e) {
                result.setResult(AdjournmentResult.ALREADY_SUSPENDED);
            }
        }

        return response;
    }

}
