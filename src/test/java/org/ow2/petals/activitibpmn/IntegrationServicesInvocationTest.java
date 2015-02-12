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
package org.ow2.petals.activitibpmn;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_GETTASKS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_TASK_SERVICE;
import static org.ow2.petals.component.framework.junit.Assert.assertMonitProviderBeginLog;
import static org.ow2.petals.component.framework.junit.Assert.assertMonitProviderEndLog;
import static org.ow2.petals.component.framework.junit.Assert.assertMonitProviderFailureLog;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;
import java.util.logging.LogRecord;

import javax.xml.bind.JAXBException;

import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.activitibpmn.incoming.integration.GetTasksOperation;
import org.ow2.petals.activitibpmn.incoming.integration.exception.EmptyRequestException;
import org.ow2.petals.activitibpmn.incoming.integration.exception.InvalidRequestException;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.impl.message.WrappedRequestToProviderMessage;
import org.ow2.petals.components.activiti.generic._1.GetTasks;
import org.ow2.petals.components.activiti.generic._1.GetTasksResponse;
import org.ow2.petals.samples.se_bpmn.vacationservice.Demande;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class IntegrationServicesInvocationTest extends AbstractComponentTest {

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is not compliant to the XML schema defined in WSDL</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     * </p>
     */
    @Test
    public void getTasks_UnexpectedRequest_0() throws Exception {

        final Demande getTasksReq = new Demande();

        IN_MEMORY_LOG_HANDLER.clear();
        COMPONENT_UNDER_TEST.pushRequestToProvider(new WrappedRequestToProviderMessage(COMPONENT_UNDER_TEST
                .getServiceConfiguration(NATIVE_SVC_CFG), ITG_OP_GETTASKS, AbsItfOperation.MEPPatternConstants.IN_OUT
                .value(), new ByteArrayInputStream(this.toByteArray(getTasksReq))));

        final ResponseMessage getTaskRespMsg = COMPONENT_UNDER_TEST.pollResponseFromProvider();
        assertNull("An error is set in the response", getTaskRespMsg.getError());
        assertNull("A XML payload is set in response", getTaskRespMsg.getPayload());
        assertNotNull("No fault in response", getTaskRespMsg.getFault());

        final String getTaskRespStr = SourceHelper.toString(getTaskRespMsg.getFault());
        assertTrue(getTaskRespStr.contains(InvalidRequestException.class.getName()));

        // Check MONIT traces
        assertMonitLogsWithFailure();
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is compliant to the XML schema defined in WSDL</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     * </p>
     */
    @Test
    public void getTasks_UnexpectedRequest_1() throws Exception {

        final GetTasksResponse getTasksReq = new GetTasksResponse();

        IN_MEMORY_LOG_HANDLER.clear();
        COMPONENT_UNDER_TEST.pushRequestToProvider(new WrappedRequestToProviderMessage(COMPONENT_UNDER_TEST
                .getServiceConfiguration(NATIVE_SVC_CFG), ITG_OP_GETTASKS, AbsItfOperation.MEPPatternConstants.IN_OUT
                .value(), new ByteArrayInputStream(this.toByteArray(getTasksReq))));

        final ResponseMessage getTaskRespMsg = COMPONENT_UNDER_TEST.pollResponseFromProvider();
        assertNull("An error is set in the response", getTaskRespMsg.getError());
        assertNull("A XML payload is set in response", getTaskRespMsg.getPayload());
        assertNotNull("No fault in response", getTaskRespMsg.getFault());

        final String getTaskRespStr = SourceHelper.toString(getTaskRespMsg.getFault());
        assertTrue(getTaskRespStr.contains(InvalidRequestException.class.getName()));

        // Check MONIT traces
        assertMonitLogsWithFailure();
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * <ul>
     * <li>an empty request is sent</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     * </p>
     */
    @Test
    public void getTasks_EmptyRequest() throws Exception {

        IN_MEMORY_LOG_HANDLER.clear();
        COMPONENT_UNDER_TEST.pushRequestToProvider(new WrappedRequestToProviderMessage(COMPONENT_UNDER_TEST
                .getServiceConfiguration(NATIVE_SVC_CFG), ITG_OP_GETTASKS, AbsItfOperation.MEPPatternConstants.IN_OUT
                .value(), null));

        final ResponseMessage getTaskRespMsg = COMPONENT_UNDER_TEST.pollResponseFromProvider();
        assertNull("An error is set in the response", getTaskRespMsg.getError());
        assertNull("A XML payload is set in response", getTaskRespMsg.getPayload());
        assertNotNull("No fault in response", getTaskRespMsg.getFault());

        final String getTaskRespStr = SourceHelper.toString(getTaskRespMsg.getFault());
        assertTrue(getTaskRespStr.contains(EmptyRequestException.class.getName()));

        // Check MONIT traces
        assertMonitLogsWithFailure();
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * <ul>
     * <li>no argument is given</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no error occurs</li>
     * <li>no fault occurs</li>
     * <li>no task returns because no process instance exists</li>
     * </ul>
     * </p>
     */
    @Test
    public void getTasks_NoArguments() throws JAXBException, IOException {

        final GetTasks getTasksReq = new GetTasks();

        IN_MEMORY_LOG_HANDLER.clear();
        COMPONENT_UNDER_TEST.pushRequestToProvider(new WrappedRequestToProviderMessage(COMPONENT_UNDER_TEST
                .getServiceConfiguration(NATIVE_SVC_CFG), ITG_OP_GETTASKS, AbsItfOperation.MEPPatternConstants.IN_OUT
                .value(), new ByteArrayInputStream(this.toByteArray(getTasksReq))));

        final ResponseMessage getTaskRespMsg = COMPONENT_UNDER_TEST.pollResponseFromProvider();
        assertNull("An error is set in the response", getTaskRespMsg.getError());
        assertNull("A fault is set in the response", getTaskRespMsg.getFault());
        assertNotNull("No XML payload in response", getTaskRespMsg.getPayload());
        final Object getTaskRespObj = UNMARSHALLER.unmarshal(getTaskRespMsg.getPayload());
        assertTrue(getTaskRespObj instanceof GetTasksResponse);
        final GetTasksResponse getTaskResp = (GetTasksResponse) getTaskRespObj;
        assertNotNull(getTaskResp.getTasks());
        assertNotNull(getTaskResp.getTasks().getTask());
        assertEquals(0, getTaskResp.getTasks().getTask().size());

        // Check MONIT traces
        final List<LogRecord> monitLogs = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        final FlowLogData providerBegin = assertMonitProviderBeginLog(ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE,
                COMPONENT_UNDER_TEST.getNativeEndpointName(ITG_TASK_SERVICE), ITG_OP_GETTASKS, monitLogs.get(0));
        assertMonitProviderEndLog(providerBegin, monitLogs.get(1));
    }

    /**
     * Assertion about MONIT logs generated during an invocation with error of the integration operation 'GetTasks'
     */
    private void assertMonitLogsWithFailure() {
        final List<LogRecord> monitLogs = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        final FlowLogData providerBegin = assertMonitProviderBeginLog(ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE,
                COMPONENT_UNDER_TEST.getNativeEndpointName(ITG_TASK_SERVICE), ITG_OP_GETTASKS, monitLogs.get(0));
        assertMonitProviderFailureLog(providerBegin, monitLogs.get(1));
    }

}
