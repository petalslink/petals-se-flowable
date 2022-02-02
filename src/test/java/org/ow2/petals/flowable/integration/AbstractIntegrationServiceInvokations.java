/**
 * Copyright (c) 2015-2022 Linagora
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
package org.ow2.petals.flowable.integration;

import java.util.List;
import java.util.logging.LogRecord;

import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.components.flowable.generic._1.InvalidRequest;
import org.ow2.petals.flowable.VacationProcessTestEnvironment;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.Demande;

public abstract class AbstractIntegrationServiceInvokations extends VacationProcessTestEnvironment {

    /**
     * <p>
     * Check the processing of an integration service operation when:
     * </p>
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is not compliant to the XML schema defined in WSDL</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    protected void testInvalidRequest_WsdlUncompliant(final String suName, final QName interfaceName,
            final QName serviceName,
            final QName operationName) throws Exception {

        final Demande request = new Demande();

        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, suName, operationName,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request));

        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(requestM);
        assertNull("An error is set in the response", responseMsg.getError());
        assertNull("An out is set in response", responseMsg.getOut());
        assertNotNull("No fault in response", responseMsg.getFault());

        final Object faultObj = UNMARSHALLER.unmarshal(responseMsg.getFault());
        assertTrue(faultObj instanceof InvalidRequest);
        final InvalidRequest invalidRequest = (InvalidRequest) faultObj;
        assertTrue(invalidRequest.getMessage().endsWith("is unexpected"));
        assertFalse(invalidRequest.getStacktrace().isEmpty());

        COMPONENT.sendDoneStatus(responseMsg);

        // Check MONIT traces
        assertMonitLogsWithFailure(interfaceName, serviceName, operationName);
    }

    /**
     * <p>
     * Check the processing of the integration service operation when:
     * </p>
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is compliant to the XML schema defined in WSDL, for example a response used as request</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    protected void testInvalidRequest_WsdlCompliant(final String suName, final QName interfaceName,
            final QName serviceName,
            final QName operationName, final Object request) throws Exception {

        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, suName, operationName,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request));

        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(requestM);
        assertNull("An error is set in the response", responseMsg.getError());
        assertNull("An out is set in response", responseMsg.getOut());
        assertNotNull("No fault in response", responseMsg.getFault());

        final Object faultObj = UNMARSHALLER.unmarshal(responseMsg.getFault());
        assertTrue(faultObj instanceof InvalidRequest);
        final InvalidRequest invalidRequest = (InvalidRequest) faultObj;
        assertTrue(invalidRequest.getMessage().endsWith("is unexpected"));
        assertFalse(invalidRequest.getStacktrace().isEmpty());

        COMPONENT.sendDoneStatus(responseMsg);

        // Check MONIT traces
        assertMonitLogsWithFailure(interfaceName, serviceName, operationName);
    }

    /**
     * <p>
     * Check the processing of the integration service operation when:
     * </p>
     * <ul>
     * <li>an empty request is sent</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    protected void testInvalidRequest_Empty(final String suName, final QName interfaceName, final QName serviceName,
            final QName operationName)
            throws Exception {

        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage request = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, suName, operationName,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), (Source) null);

        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(request);
        assertNull("An error is set in the response", responseMsg.getError());
        assertNull("An out is set in response", responseMsg.getOut());
        assertNotNull("No fault in response", responseMsg.getFault());

        final Object faultObj = UNMARSHALLER.unmarshal(responseMsg.getFault());
        assertTrue(faultObj instanceof InvalidRequest);
        final InvalidRequest invalidRequest = (InvalidRequest) faultObj;
        assertTrue(invalidRequest.getMessage().endsWith("is empty"));
        assertFalse(invalidRequest.getStacktrace().isEmpty());

        COMPONENT.sendDoneStatus(responseMsg);

        // Check MONIT traces
        assertMonitLogsWithFailure(interfaceName, serviceName, operationName);
    }

    /**
     * <p>
     * Check the processing of the integration service operation with the given request. No error and no fault expected.
     */
    protected Object testRequest(final String suName, final QName interfaceName,
            final QName serviceName, final QName operationName, final Object request) throws Exception {

        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, suName, operationName,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request));

        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(requestM);

        assertNull("An error is set in the response", responseMsg.getError());
        assertNull("A fault is set in the response", responseMsg.getFault());
        assertNotNull("No XML payload in response", responseMsg.getPayload());
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());

        COMPONENT.sendDoneStatus(responseMsg);

        // Check MONIT traces
        final List<LogRecord> monitLogs = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        final FlowLogData providerBegin = assertMonitProviderBeginLog(interfaceName, serviceName,
                COMPONENT_UNDER_TEST.getNativeEndpointName(serviceName), operationName, monitLogs.get(0));
        assertMonitProviderEndLog(providerBegin, monitLogs.get(1));

        return responseObj;
    }

    /**
     * <p>
     * Check the processing of the integration service operation with the given request. Fault expected.
     */
    protected Object testRequestWithFault(final String suName, final QName interfaceName, final QName serviceName,
            final QName operationName, final Object request) throws Exception {

        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM = new RequestToProviderMessage(COMPONENT_UNDER_TEST, suName,
                operationName, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request));

        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(requestM);

        assertNull("An error is set in the response", responseMsg.getError());
        assertTrue("A XML payload is set in the response", responseMsg.isFault());
        assertNotNull("No fault in response", responseMsg.getFault());
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getFault());

        COMPONENT.sendDoneStatus(responseMsg);

        // Check MONIT traces
        final List<LogRecord> monitLogs = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        final FlowLogData providerBegin = assertMonitProviderBeginLog(interfaceName, serviceName,
                COMPONENT_UNDER_TEST.getNativeEndpointName(serviceName), operationName, monitLogs.get(0));
        assertMonitProviderFailureLog(providerBegin, monitLogs.get(1));

        return responseObj;
    }

    /**
     * Assertion about MONIT logs generated during an invocation with error of the integration operation
     */
    private void assertMonitLogsWithFailure(final QName interfaceName, final QName serviceName,
            final QName operationName) {
        final List<LogRecord> monitLogs = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        final FlowLogData providerBegin = assertMonitProviderBeginLog(interfaceName, serviceName,
                COMPONENT_UNDER_TEST.getNativeEndpointName(serviceName), operationName, monitLogs.get(0));
        assertMonitProviderFailureLog(providerBegin, monitLogs.get(1));
    }

}
