/**
 * Copyright (c) 2018-2022 Linagora
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

import static org.junit.Assert.assertNotNull;

import java.util.HashMap;
import java.util.Map;

import javax.xml.ws.WebServiceException;

import org.flowable.engine.runtime.Execution;
import org.flowable.engine.runtime.ExecutionQuery;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.test.FlowableRule;
import org.ow2.petals.samples.se_flowable.collaboration.services.Collaboration;
import org.ow2.petals.samples.se_flowable.collaboration.services.MasterProcessAlreadyNotified_Exception;
import org.ow2.petals.samples.se_flowable.collaboration.services.MasterProcessUnknown_Exception;
import org.ow2.petals.samples.se_flowable.collaboration.services.NoMasterProcessWaitingNotification_Exception;

public class CollaborationSvcMock implements Collaboration {

    private final FlowableRule flowableRule;

    public CollaborationSvcMock(final FlowableRule flowableRule) {
        this.flowableRule = flowableRule;
    }

    @Override
    public Object startChildProcess(final String processInstanceIdCallback) {

        final Map<String, Object> variables = new HashMap<>();
        variables.put("processInstanceIdCallback", processInstanceIdCallback);
        final ProcessInstance childProcInst = this.flowableRule.getRuntimeService()
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

        final ExecutionQuery query = this.flowableRule.getRuntimeService().createExecutionQuery()
                .processInstanceId(processInstanceIdCallback).messageEventSubscriptionName("notifyMasterProcessMsg");
        final Execution execution = query.singleResult();
        assertNotNull(execution);

        this.flowableRule.getRuntimeService().messageEventReceived("notifyMasterProcessMsg", execution.getId());
    }

}
