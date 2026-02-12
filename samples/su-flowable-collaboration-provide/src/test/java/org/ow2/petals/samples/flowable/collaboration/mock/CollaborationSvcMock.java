/**
 * Copyright (c) 2018-2026 Linagora
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
package org.ow2.petals.samples.flowable.collaboration.mock;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.HashMap;
import java.util.Map;

import org.flowable.engine.ProcessEngine;
import org.flowable.engine.runtime.Execution;
import org.flowable.engine.runtime.ExecutionQuery;
import org.flowable.engine.runtime.ProcessInstance;
import org.ow2.petals.samples.se_flowable.collaboration.services.Collaboration;
import org.ow2.petals.samples.se_flowable.collaboration.services.MasterProcessAlreadyNotified_Exception;
import org.ow2.petals.samples.se_flowable.collaboration.services.MasterProcessUnknown_Exception;
import org.ow2.petals.samples.se_flowable.collaboration.services.NoMasterProcessWaitingNotification_Exception;

import jakarta.xml.ws.WebServiceException;

public class CollaborationSvcMock implements Collaboration {

    private final ProcessEngine processEngine;

    public CollaborationSvcMock(final ProcessEngine processEngine) {
        this.processEngine = processEngine;
    }

    @Override
    public Object startChildProcess(final String processInstanceIdCallback) {

        final Map<String, Object> variables = new HashMap<>();
        variables.put("processInstanceIdCallback", processInstanceIdCallback);
        final ProcessInstance childProcInst = this.processEngine.getRuntimeService()
                .startProcessInstanceByMessage("startChildProcessMsg", variables);
        return childProcInst.getId();
    }

    @Override
    public Object startMasterProcess() {
        // No operation, its the process startup
        return null;
    }

    @Override
    public void notifyMasterProcess(final String processInstanceIdCallback, final String dataReturned)
            throws MasterProcessAlreadyNotified_Exception, MasterProcessUnknown_Exception,
            NoMasterProcessWaitingNotification_Exception {

        if (processInstanceIdCallback == null || processInstanceIdCallback.trim().isEmpty() || dataReturned == null
                || dataReturned.trim().isEmpty()) {
            throw new WebServiceException(
                    String.format("Missing or empty parameter: processInstanceIdCallback='%s', dataReturned='%s'",
                            processInstanceIdCallback, dataReturned));
        }

        try {
            // Leave time to commit first activities of master process instance
            Thread.sleep(5000);
        } catch (final InterruptedException e) {
            throw new WebServiceException(e);
        }

        final ExecutionQuery query = this.processEngine.getRuntimeService().createExecutionQuery()
                .processInstanceId(processInstanceIdCallback).messageEventSubscriptionName("notifyMasterProcessMsg");
        final Execution execution = query.singleResult();
        assertNotNull(execution);

        this.processEngine.getRuntimeService().messageEventReceived("notifyMasterProcessMsg", execution.getId());
    }
}
